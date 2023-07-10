{-# Language OverloadedStrings #-}
module Reflex.Process.Lines where

import Control.Monad.Fix (MonadFix)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Reflex

-- * Output lines

-- | Accumulator for line-based output that keeps track of any dangling,
-- unterminated line
data Lines = Lines
  { _lines_terminated :: Seq C8.ByteString
  , _lines_unterminated :: Maybe C8.ByteString
  }
  deriving (Show, Eq, Ord, Read)

-- | Empty output
emptyLines :: Lines
emptyLines = Lines Seq.empty Nothing

-- | Add some raw output to a 'Lines'. This will chop the raw output up into lines.
addLines :: ByteString -> Lines -> Lines
addLines new (Lines t u) =
  let newLines' = Seq.fromList $ filter (not . C8.null) (C8.lines new)
  in
    case u of
      Nothing -> if "\n" `C8.isSuffixOf` new
        then Lines (t <> newLines') Nothing
        else case Seq.viewr newLines' of
                Seq.EmptyR -> Lines t Nothing
                (t' Seq.:> u') -> Lines (t <> t') (Just u')
      Just u' -> addLines (u' <> new) $ Lines t Nothing

-- | Convert a 'ByteString' into a 'Lines'
linesFromBS :: C8.ByteString -> Lines
linesFromBS = flip addLines mempty

instance Semigroup Lines where
  a <> b = addLines (unLines b) a

instance Monoid Lines where
  mempty = emptyLines

-- | Convert a 'Lines' back into a 'ByteString'
unLines :: Lines -> ByteString
unLines (Lines t u) =
  C8.unlines (toList t) <> fromMaybe "" u

-- | Convenience accessor for the last whole line received by a 'Lines'.
-- Ignores any unterminated line that may follow.
lastWholeLine :: Lines -> Maybe C8.ByteString
lastWholeLine (Lines t _) = case Seq.viewr t of
  Seq.EmptyR -> Nothing
  _ Seq.:> x -> Just x

-- | Split lines into two. The sequence that satisfies the predicate is
-- consumed and will not appear in either resulting 'Lines'.
splitLinesOn :: (ByteString -> Bool) -> Lines -> Maybe (Lines, Lines)
splitLinesOn test (Lines t u) = 
  let (before, after) = Seq.breakl test t
  in if Seq.null after then Nothing else Just (Lines before Nothing, Lines (Seq.drop 1 after) u)

-- | Given an event of raw bytes, fire an output event of *terminated* lines.
-- Unterminated lines are held until the line they belong to is completed or
-- until the flush event fires.
newLines
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t ByteString
  -> Event t () -- ^ Event that flushes any remaining unterminated lines
  -> m (Event t Lines) -- ^ These will be complete lines except when the flush event fires, in which it may include unterminated lines
newLines e flush = do
  x <- foldDyn ($) (mempty, mempty) $ mergeWith (.)
    [ ffor e $ \new (_, old) ->
        let Lines t u = addLines new old
        in (Lines t Nothing, Lines mempty u)
    , ffor flush $ \_ (_, old) -> (old, emptyLines)
    ]
  pure $ fforMaybe (updated x) $ \(terminatedLines, _) -> if terminatedLines == mempty
    then Nothing
    else Just terminatedLines
