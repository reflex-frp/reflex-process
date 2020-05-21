# Revision history for reflex-process

## 0.3.0.0

* ([#11](https://github.com/reflex-frp/reflex-process/pull/11)) Add `createProcessBufferingInput` for buffering input to processes and change `createProcess` to use an unbounded buffer instead of blocking the FRP network when the process blocks on its input handle.
* ([#11](https://github.com/reflex-frp/reflex-process/pull/11), [#14](https://github.com/reflex-frp/reflex-process/pull/14)) `ProcessConfig` now includes a `_processConfig_createProcess` field for customizing how the process is created.
* ([#13](https://github.com/reflex-frp/reflex-process/pull/13)) Fix race condition between process completion `Event`s and process `stdout`/`stderr` `Event`s. Process completion is now always the very last `Event` to fire for a given `Process`.
* ([#15](https://github.com/reflex-frp/reflex-process/pull/15), [#13](https://github.com/reflex-frp/reflex-process/pull/13)) **(Breaking change)** Introduce `SendPipe` type for encoding when an input stream should send EOF and change `createProcess` to take a `ProcessConfig t (SendPipe ByteString)` so that sending EOF is possible.
* ([#14](https://github.com/reflex-frp/reflex-process/pull/14)) **(Breaking change)** Consolidate `ProcessConfig` and `CreateProcess` which has the following effects:
  * Instead of constructing `ProcessConfig` with `def` you now use `defProcessConfig` and provide your `CreateProcess` here.
  * `createRedirectedProcess` was renamed to `createProcessWith` and now takes a single `ProcessConfig` as normally constructed by `defProcessConfig`.
  * `createProcess` now takes a single `ProcessConfig` as normally constructed by `defProcessConfig`.

  For example, if you had `createProcess cmd def`, you would change it to `createProcess (defProcessConfig cmd)`.


## 0.2.1.0

* `createProcess`: Ensure that handle is open before attempting to check whether it is readable

## 0.2.0.0

* Breaking change: Generalise input and output parameters of createRedirectedProcess. Existing programs should replace `Process t` with `Process t ByteString ByteString` and `ProcessConfig t` with `ProcessConfig t ByteString`.

## 0.1.0.1

* Loosen reflex-vty version bounds

## 0.1.0.0

* Initial release. The core of the interface is `Reflex.Process.createProcess`, which runs a `System.Process.CreateProcess` command, taking `Event`s of input and producing `Event`s of output.
