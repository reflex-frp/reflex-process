# Revision history for reflex-process

## 0.3.2.1

* Allow reflex-vty 0.5.*

## 0.3.2.0

* Derive Eq, Ord, and Show instances for SendPipe

## 0.3.1.2-r1

* Loosen version bounds to support reflex-0.9 and reflex-vty 0.4

## 0.3.1.2

* Update readme example to support reflex-vty 0.3

## 0.3.1.1

* Update readme example to use reflex-vty 0.2

## 0.3.1.0

* Support reflex 0.8
* Fix a handle leak (#23) and a thread leak (#24)

## 0.3.0.0

* ([#15](https://github.com/reflex-frp/reflex-process/pull/15), [#13](https://github.com/reflex-frp/reflex-process/pull/13)) **(Breaking change)** Introduce `SendPipe` type for encoding when an input stream should send EOF. Change `createProcess` to take a `ProcessConfig t (SendPipe ByteString)` so that sending EOF is possible.
  * **IMPORTANT**: For `createProcess` messages to `stdin` must now be wrapped in `SendPipe_Message` *and* have a `"\n"` manually appended to regain the old behavior. Previously `createProcess` implicitly added a `"\n"` to all messages sent to the process. This has been removed and *you must now manually add any necessary new lines to your messages.* This change allows `createProcess` to work with processes in a encoding-agnostic way on `stdin`.
* ([#17](https://github.com/reflex-frp/reflex-process/pull/17)) Deprecate `createRedirectedProcess` in favor of a new, scarier name: `unsafeCreateProcessWithHandles`. This was done to communicate that it does not enforce necessary guarantees for the process to be handled correctly.
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
