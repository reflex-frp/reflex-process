# Revision history for reflex-process

## Unreleased

* Generalise input and output parameters of createRedirectedProcess

## 0.1.0.1

* Loosen reflex-vty version bounds

## 0.1.0.0

* Initial release. The core of the interface is `Reflex.Process.createProcess`, which runs a `System.Process.CreateProcess` command, taking `Event`s of input and producing `Event`s of output.
