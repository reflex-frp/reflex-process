# Revision history for reflex-process

## Unreleased

* ([#15](https://github.com/reflex-frp/reflex-process/pull/15), [#13](https://github.com/reflex-frp/reflex-process/pull/13)) **(Breaking change)** Introduce `SendPipe` type for encoding when an input stream should send EOF and change `createProcess` to take a `ProcessConfig t (SendPipe ByteString)` so that sending EOF is possible.
  * **IMPORTANT**: `SendPipe_Message` is not strictly equivalent to the prior behavior. Prior to this change, `createProcess` always added a new line to all messages sent to the process. This implicit new line has been removed and *you must now add any new lines to your messages manually.* This implicit change allows `createProcess` to work with processes that truly take binary data on `stdin`.
* ([#17](https://github.com/reflex-frp/reflex-process/pull/17)) **(Breaking change)** Rename `createRedirectedProcess` to `unsafeCreateProcessWithHandles` to communicate that it does not enforce necessary guarantees for the process to be handled correctly.
* ([#15](https://github.com/reflex-frp/reflex-process/pull/15), [#13](https://github.com/reflex-frp/reflex-process/pull/13)) **(Breaking change)** Sending a message with `SendPipe_Message` `createProcess`
* ([#11](https://github.com/reflex-frp/reflex-process/pull/11)) Add `createProcessBufferingInput` for buffering input to processes and change `createProcess` to use an unbounded buffer instead of blocking the FRP network when the process blocks on its input handle.
* ([#11](https://github.com/reflex-frp/reflex-process/pull/11), [#14](https://github.com/reflex-frp/reflex-process/pull/14)) `ProcessConfig` now includes a `_processConfig_createProcess` field for customizing how the process is created.
* ([#13](https://github.com/reflex-frp/reflex-process/pull/13)) Fix race condition between process completion `Event`s and process `stdout`/`stderr` `Event`s. Process completion is now always the very last `Event` to fire for a given `Process`.
* ([#17](https://github.com/reflex-frp/reflex-process/pull/17)) Add `defProcessConfig` to avoid forcing users to depend on `data-default`.


## 0.2.1.0

* `createProcess`: Ensure that handle is open before attempting to check whether it is readable

## 0.2.0.0

* Breaking change: Generalise input and output parameters of createRedirectedProcess. Existing programs should replace `Process t` with `Process t ByteString ByteString` and `ProcessConfig t` with `ProcessConfig t ByteString`.

## 0.1.0.1

* Loosen reflex-vty version bounds

## 0.1.0.0

* Initial release. The core of the interface is `Reflex.Process.createProcess`, which runs a `System.Process.CreateProcess` command, taking `Event`s of input and producing `Event`s of output.
