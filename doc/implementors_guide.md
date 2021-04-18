# AtomVM Implementors Guide

This guide is designed for AtomVM implementors, developers who are

## Building AtomVM

AtomVM can be built via the command line and can target numerous environments, including Linux, FreeBSD, MacOS, ESP32, and STM32.

In the case of embedded devices (ESP32 and STM32), toolchains from open source and third-party vendors are required in order to cross-compile AtomVM source code (most of which is written in the C programming language)

### Downloading AtomVM

At present, AtomVM is only available via a checkout of the AtomVM github repository.

> Note.  Downloading the AtomVM github repository requires the installtion of the `git` program.  Consult your local OS documentation for installtion of the `git` package.

	shell$ git clone https://github.com/bettio/AtomVM
	TODO
	shell$ cd AtomVM


### Generic Unix

The following instructions apply to unix-like environments, including Linux, FreeBSD, and MacOS.

#### Pre-requisites

The following software is required in order to build AtomVM in generic UNIX systems:

* `gcc` or `llvm` tool chains
* `cmake`
* `make`
* `gperf`
* `zlib`

Consult your local OS documentation for instructions about how to install these components.

#### Build Instructions

The AtomVM build for generic UNIX systems makes use of the `cmake` tool for generating `make` files from the top level AtomVM directory, as follows:

	shell$ mkdir build
	shell$ cd build
	shell$ cmake ..
	...
	shell$ make

> Note.  You may optionally specify `-j <n>`, where `<n>` is the number of CPUs you would like to assign to run the build in parallel.

Upon completion, the `AtomVM` execcutable can be found in the `build/src` directory.

#### Special Note for MacOS users

You may build an Apple Xcode project, for developing, testing, and debugging in the Xcode IDE, by specifying the Xcode generator.  For example, from the top level AtomVM directory:

	shell$ mkdir xcode
	shell$ cmake -G Xcode ..
	...
	shell$ open AtomVM.xcodeproj

The above commands will build and open an AtomVM project in the Xcode IDE.

### ESP32




#### Pre-requisites

The following software is required in order to build AtomVM:

* Espressif Xtensa tool chains
* Espressif IDF toolkit
* `cmake`
* GNU `make`

#### Build Instructions




## Targeting AtomVM



## AtomVM Components

### Bytecode Interpreter

### Scheduler

### Memory Management

#### Garbage Collection

### Code Loading

### BIFs

### NIFs

### Ports

### PackBEAM

## AtomVM Platforms

### Generic Unix

### ESP32

### STM32

### Targeting AtomVM for a new platform

## AtomVM Libraries

## Resources
