# AtomVM {#mainpage}

Welcome to AtomVM, the Erlang virtual machine for IoT devices!

AtomVM is designed to allow developers to implement applications in the Erlang and Elixir (among other) programming languages, and to deploy those applications onto tiny devices.  Currently, AtomVM supports the [Espressif](https://www.espressif.com) [ESP32](https://www.espressif.com/en/products/socs/esp32) micro-controller, as well as the [ST Microelectronics](https://www.st.com/content/st_com/en.html) [STM32](https://www.st.com/en/microcontrollers-microprocessors/stm32-32-bit-arm-cortex-mcus.html).  Users may also target their applications for fully-fledged operating systems (Linux, FreeBSD, MacOS), though in most cases deployment to traditional computers is done for development and testing purposes, only.

AtomVM implements the "Bogdan Erlang Abstract Machine" (hereafter, the BEAM), the underlying runtime for the Erlang and Elixir (among other) BEAM-targeted programming environments.  As an implementation of the BEAM, AtomVM supports lightweight "processes", with message passing as the mechanism for inter-(erlang)process communication, pre-emptive multi-tasking, and per-process heap allocation and garbage collection.  As such, AtomVM provides a modern, memory managed, and concurrency-oriented environment for developing applications on small devices.

> Note.  While the BEAM is a mature and fully featured collection of software, it is worth noting that there is no general specification for the VM.  In most cases, the current implementation is the definitive specification of VM behavior, and AtomVM follows the implementation of the BEAM, insofar as possible.

AtomVM is designed to run unmodified `.beam` files that have been produced by the Erlang and Elixir development tool chains (`erlc`, `rebar`, `mix`, etc).  Developers can therefore make use of the tools they are used to for Erlang and Elixir development, and use AtomVM tooling to deploy those applications to various micro-controllers.

AtomVM provides a strict subset of functionality of the BEAM.  Part of this is because AtomVM is under active development.  A more important reason, however, is that the environments on which AtomVM applications are deployed are significantly more constrained than typical BEAM environments.  For example, most micro-controller environments do not support native POSIX APIs for interfacing with an operating system, and in many cases, common operating system abstractions, such as processes, threads, or files, are simply unavailable.  AtomVM, therefore, aims to provide a subset of functionality that is appropriate for deployment to environments on which it is deployed.

However, because the BEAM is a pre-emptive multitasking environment for your code, many of the common operating system abstractions, particularly around concurrency, are simply not needed.  You can write your Erlang or Elixir code as you ordinarily would for a multi-core system architecture, and AtomVM will run your code, accordingly.  This makes writing concurrent code for micro-controllers (e.g., and application that reads sensor data, services HTTP requests, and updates the system clock, all at the same time) incredibly simple and natural.  Many programming environments struggle to provide such features without extreme contortions to the language.

In addition, AtomVM provides interfaces for integrating with features commonly seen on micro-controllers, such as GPIO pins, analog-to-digital conversion, and common industry peripheral interfaces, such as I2C, SPI, and UART, making AtomVM a serious contender for developing rich IoT applications.

Finally, one of the exciting aspects about modern micro-controllers, such as the ESP32, is their integration with modern networking technologies, such as WiFi and Bluetooth.  AtomVM takes advantage of these integrations on the platforms on which they are offered, opening up further possibilities for developing networked and wireless IoT devices.

We think you will agree that AtomVM provides a compelling environment not only for Erlang and Elixir development, but also as a home for complex and interesting IoT projects.

# Design Philosophy

AtomVM is designed from the start to run on small, cheap embedded devices, where system resources (memory, cpu, storage) are tightly constrained.  The smallest environment in which AtomVM runs has around 256k of addressable RAM, some of which is used by the underlying runtime (FreeRTOS), and some of which is used by the AtomVM virtual machine, itself, leaving even less RAM for your own applications.  Where there is a tradeoff between memory consumption and performance, minimizing memory consumption (and heap fragmentation) always wins.

From the developer's point of view, AtomVM is designed to make use of the existing tool chain from the Erlang and Elixir ecosystems.  This includes the Erlang and Elixir compilers, which will compile Erlang and Elixir source code to BEAM bytecode.  Where possible, AtomVM makes use of existing tool chains to reduce the amount of unnecessary features in AtomVM, thus reducing complexity, as well as the amount of system resources in use by the runtime.  AtomVM is designed to be as small and lean as possible, providing as many resources to user applications, as possible.

# Licensing

AtomVM is licensed under the terms of the [LGPLv2](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html) and [Apache2](https://www.apache.org/licenses/LICENSE-2.0) licenses.

# Contributing

The AtomVM community welcomes contributions tot he AtomVM code base and upstream and downstream projects.  Please see the [contributing guidelines](../CONTRIBUTING.md) for information about how to contribute.

AtomVM developers can be reached on the #AtomVM discord server.

# Getting Started

The following guides provide more detailed information about getting started with the AtomVM virtual machine, how to develop and deploy applications, and implementation information, for anyone interested in getting more involved:

* [Getting Started Guide](getting_started_guide.md)
* [Example Programs](example_programs.md)
* [Programmers Guide](programmers_guide.md)
* [Implementors Guide](implementors_guide.md)
