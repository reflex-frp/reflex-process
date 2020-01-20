# Revision history for reflex-process

## Unreleased

* Breaking change: Generalise input and output parameters of createRedirectedProcess. Existing programs should replace `Process t` with `Process t ByteString ByteString` and `ProcessConfig t` with `ProcessConfig t ByteString`.

## 0.1.0.1

* Loosen reflex-vty version bounds

## 0.1.0.0

* Initial release. The core of the interface is `Reflex.Process.createProcess`, which runs a `System.Process.CreateProcess` command, taking `Event`s of input and producing `Event`s of output.
