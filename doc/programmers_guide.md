# AtomVM Programmers Guide

This guide is intended for programmers who develop applications targeted for AtomVM.

As an implementation of the Erlang virtual machine, AtomVM is designed to execute unmodified byte-code instructions compiled into BEAM files, either by the Erlang or Elixir compilers.  This allow developers to write programs in their programming language of choice, and to use the common Erlang community tool-chains specific to their language platform, and to then deploy those applications onto the various devices that AtomVM supports.

This document describes the development workflow when writing AtomVM applications, as well as a high-level overview of the various APIs that are supported by AtomVM.  With an understanding of this guide, you should be able to design, implement, and deploy applications onto a device running the AtomVM virtual machine.

## Development Overview

This section describes the typical development environment and workflow most AtomVM developers are most likely to use.

### Development Environment

In general, for most development purposes, you should be able to get away with an Erlang/OTP development environment (OTP21 or later), and for Elixir developers, and Elixir version TODO development environment.  We assume most development will take place on some UNIX-like environment (e.g., Linux, FreeBSD, or MacOS).  Consult your local package manager for installation of these development environments.

Developers will want to make use of common Erlang or Elixir development tools, such as `rebar3` for Erlang developers or `mix` for Elixir developers.

Developers will need to make use of some AtomVM tooling

* AtomVM `packbeam` executable (described below)
* (Optional) `atomvm_rebar3_plugin`, for development using `rebar3`
* (Optional)

Some testing can be performed on UNIX-like systems, using the `AtomVM` executable that is suitable for your development environment.  AtomVM applications that do not make use of platform-specific APIs are suitable for such tests.

Deployment and testing on micro-controllers is slightly more involved, as these platforms require additional hardware and software, described below.

#### ESP32 Deployment

In order to deploy AtomVM applications to and test on the ESP32 platform, developers will need:

* A computer running MacOS or Linux (Windows support is TBD);
* An ESP32 module with a USB/UART connector (typically part of an ESP32 development board);
* A USB cable capable of connecting the ESP32 module or board to your development machine (laptop or PC);
* The [`esptool`](https://github.com/espressif/esptool) program, for flashing the AtomVM image and AtomVM programs;
* (Optional, but recommended) A serial console program, such as `minicom` or `screen`, so that you can view console output from your AtomVM application.


#### STM32 requirements

TODO

### Development Workflow

For the majority of users, AtomVM applications are written in the Erlang or Elixir programming language.  These applications are compiled to BEAM (`.beam`) files using standard Erlang or Elixir compiler tool chains (`erlc`, `rebar`, `mix`, etc).  The generated BEAM files contain byte-code that can be executed by the Erlang/OTP runtime, or by the AtomVM virtual machine.

> Note.  In a small number of cases, it may be useful to write parts of an application in the C programming language, as AtomVM nifs or ports.  However, writing AtomVM nifs and ports is outside of the scope of this document.  See the [AtomVM Implementors Guide](TODO) for information about how to implement nifs and ports.

Once Erlang and/or Elixir files are compiled to BEAM files, AtomVM provides tooling for aggregating BEAM files into AtomVM Packbeam (`.avm`) files, using AtomVM tooling, distributed as part of AtomVM, or as provided through the AtomVM community.

AtomVM packbeam files are the "execution units" of the AtomVM virtual machine.  For micro-controller devices, they are "flashed" or uploaded to the device; for command-line use of AtomVM (e.g., on Linux, FreeBSD, or MacOS), they are supplied as the first parameter to the AtomVM command.

The following diagram illustrates the typical development workflow, starting from Erlang or Elixir source code, and resulting in a deployed Packbeam file:


        *.erl or *.ex                    *.beam                  app.avm
        +-------+                     +-------+                 +-------+
        |       |+                    |       |+                |       |
        |       ||+                   |       ||+               |       |
        |       |||     -------->     |       |||   -------->   |       |
        |       |||  Erlang/Elixir    |       |||     AtomVM    |       |
        +-------+||     Compiler      +-------+||     Tooling   |       |
         +-------+|                    +-------+|               +-------+
          +-------+                     +-------+                   |
                                                                    | flash/upload
                                                                    |
                                                                    v
                                                            Micro-controller
                                                                 device


The typical compile-test-debug cycle can be summarized in the following steps:

1. Deploy the AtomVM virtual machine to your device
1. Develop an AtomVM application in Erlang or Elixir
    1. Write application
    1. Deploy application to device
    1. Test/Debug/Fix application
    1. Repeat

Deployment of the AtomVM virtual machine and an AtomVM application currently require a USB serial connection.  There is currently no support for over-the-air (OTA) updates.


## AtomVM Applications

An AtomVM application is a collection of BEAM files, aggregated into an AtomVM "Packbeam" (`.avm`) file, and typically deployed (flashed) to some device.  These BEAM files be be compiled from Erlang, Elixir, or any other language that targets the Erlang VM.

> Note.  The return value from the `start/0` function is ignored.

Here, for example is one of the smallest AtomVM applications you can write:

    %% erlang
    -module(myapp).

    -export([start/0]).

    start() ->
        ok.

This particular application doesn't do much, of course.  The application will start and immediately terminate, with a return value of `ok`.  Typical AtomVM applications will be more complex than this one, and the AVM file that contains the application BEAM files will be considerably larger and more complex than the above program.

Moreover, most applications will spawn processes, send and receiive messages between processes, and
wait for certain conditions to apply before terminating, if they terminate at all.  For applications
that spawn processes and run forever, you may need to add an empty `receive ... end` block, to
prevent the AtomVM from terminating prematurely, e.g.,

    %% erlang
    wait_forever() ->
        receive X -> X end.


### Packbeam files

AtomVM applications are packaged into Packbeam (`.avm`) files, which contain collections of files, typically BEAM (`.beam`) files that have been generated by the Erlang or Elixir compiler.

At least one BEAM module in this file must contain an exported `start/0` function.  The first module in a Packbeam file that contain this function is the entry-point of your application and will be executed when the AtomVM virtual machine starts.

Not all files in a Packbeam need to be BEAM modules -- you can embed any type of file in a Packbeam file, for consumption by your AtomVM application.

> Note.  The Packbeam format is described in more detail in the AtomVM [Implementors Guide](implementors_guide.md).

The AtomVM community has provided several tools for simplifying your experience, as a developer.  These tools allow you to use standard Erlang and Elixir tooling (such as `rebar3` and `mix`) to build Packbeam files and deploy then to your device of choice.

### `packbeam` tool

The `packbeam` tool is a command-line application that can be used to create Packbeam files from a collection of input files.


## Language Features

Currently, AtomVM implements a strict subset of the BEAM instruction set, as of Erlang/OTP R21.  Previous versions of Erlang/OTP are not supported.

A high level overview of the supported language features include:

* All the major Erlang types, including
    * integers (with size limits)
    * limited support for floats (not supported on all platforms)
    * tuples
    * lists
    * binaries
    * maps
* support for many Erlang BIFs and guard expressions to support the above types
* pattern matching (case statements, function clause heads, etc)
* `try ... catch ... finally` constructs
* anonymous functions
* process `spawn` and `spawn_link`
* send (`!`) and `receive` messages
* bit syntax (with some restrictions)
* reference counted binaries

While the list of supported features is long and growing, the currently unsupported Erlang/OTP and BEAM features include (but are not limited to):

* Bingnums (integers > 32 bits)
* SMP support
* SSL support
* `epmd` and the `disterl` protocol
* Hot swapping of code
* Numerous modules and functions from Erlang/OTP standard libraries (`kernel`, `stdlib`, `sasl`, etc)

As such, it is highly unlikely that an existing Erlang program targeted for Erlang/OTP R20 will run unmodified on AtomVM.  And indeed, even as AtomVM matures and additional features are added, it is more likley than not that Erlang applications will need to targeted speficially for the AtomVM platform.  The intended target environment (small, cheap micro-controllers) differs enough from desktop or server-class systems in both scale and APIs that special care and attention is needed to target applications for such embedded environments.

That being said, many of the features of the BEAM are supported and provide a rich and compelling development environment for embedded devices, which Erlang and Elixir developers will find natural and productive.

## ESP32 Features

* UDP and TCP/IP support (`inet`, `gen_tcp` and `gen_udp`)
* Peripheral and system support on micro-controllers, including
    * Wifi networking (`network`)
    * GPIO
    * I2C
    * SPI
    * UART
    * LEDC (PWM)
    * ADC
    * non-volatile storage
    * deep sleep
    * real-time clock storage


## AtomVM APIs

The AtomVM virtual machine provides a set of Erlang built-in functions (BIFs) and native functions (NIFs), as well as a collection of Erlang and Elixir libraries that can be used from your applications.

This section provides an overview of these APIs.  For more detailed information about specific APIs, please consult the [API reference documentation](TODO).

### Standard Libraries

AtomVM provides a limited implementations of standard library modules, including:

* `base64`
* `gen_server`
* `gen_statem`
* `io` and `io_lib`
* `lists`
* `maps`
* `proplists`
* `supervisor`
* `timer`

In addition AtomVM provides limited implementations of standard Elixir modules, including:

* `List`
* `Tuple`
* `Enum`
* `Kernel`
* `Module`
* `Process`
* `Console`

For detailed information about these function, please consult the [API reference documentation](TODO).  These modules provide a strict subset of functionality from their Erlang/OTP counterparts.  However, they aim to be API-compatible with the Erlang/OTP interfaces, at least for the subset of provided functionality.

### Console Output

There are several mechanisms for writing data to the console.

For common debugging, many users will find `erlang:display/1` sufficient for debugging:

    %% erlang
    erlang:display({foo, [{bar, tapas}]}).

The output parameter is any Erlang term, and a newline will be appended automatically.

Users may prefer using the `io:format/1,2` functions for more controlled output:

    %% erlang
    io:format("The ~p did a ~p~n", [friddle, frop]).

Note that the `io_lib` module can be used to format string data, as well.

> Note. Formatting parameters are currently limited to `~p`, `~s`, and `~n`.

### Process Management

You can obtain a list of all processes in the system via `erlang:processes/0`:

    %% erlang
    Pids = erlang:processes().

And for each process, you can get detailed process information via the `erlang:process_info/1` function:

    %% erlang
    [io:format("Process info for Pid ~p: ~p~n", [Pid, erlang:process_info(Pid)]) || Pid <- Pids].

The return value is a property list containing values for `heap_size`, `stack_size`, `message_queue_len`,and `memory`.

### System APIs

You can obtain system information about the AtomVM virtual machine via the `erlang:system_info/1` function, which takes an atom parameter designating the desired datum.  Allowable parameters include

* `process_count` The number of processes running in the system.
* `port_count` The number of ports running in the system.
* `atom_count` The number of atoms allocated in the system.
* `word_size` The word size (in bytes) on the current platform (typically 4 or 8).

For example,

    %% erlang
    io:format("Atom Count: ~p~n", [erlang:system_info(atom_count)]).

> Note.  Additional platform-specific information is supported, depending on the platform type.  See below.

Use the `atomvm:platform/0` to obtain the system platform on which your code is running.  The return value of this function is an atom who's value will depend on the platform on which your application is running.

    %% erlang
    case atomvm:platform() of
        esp32 ->
            io:format("I am running on an ESP32!~n");
        stm32 ->
            io:format("I am running on an STM32!~n");
        generic_unix ->
            io:format("I am running on a UNIX box!~n")
    end.

Use `erlang:garbage_collect/0` or `erlang:garbage_collect/1` to force the AtomVM garbage collector to run on a give process.  Garbage collection will in general happen automatically when additional free space is needed and is rarely needed to be called explicitly.

The 0-arity version of this function will run the garbage collector on the currently executing process.

    %% erlang
    Pid = ... %% get a reference to some pid
    ok = erlang:garbage_collect(Pid).

### System Time

AtomVM supports numerous function for accessing the current time on the device.

Use `erlang:timestamp/0` to get the current time since the UNIX epoch (Midnight, Jan 1, 1970, UTC), at microsecond granularity, expressed as a triple (mega-seconds, seconds, and micro-seconds):

    %% erlang
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp().

User `erlang:system_time/1` to obtain the seconds or milliseconds since the UNIX epoch (Midnight, Jan 1, 1970, UTC):

    %% erlang
    Seconds = erlang:system_time(second).
    MilliSeconds = erlang:system_time(millisecond).

Use `erlang:universaltime/0` to get the current time at second resolution, to obtain the year, month, day, hour, minute, and second:

    %% erlang
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime().

> Note.  Setting the system time is done in a platform-specific manner.  For information about how to set system time on the ESP32, see the section below on SNTP.

### Miscellaneous

Use the `erlang:md5/1` function to compute the MD5 hash of an input binary.  The output is a fixed-length binary ()

    %% erlang
    Hash = erlang:md5(<<foo>>).

Use `atomvm:random/0` to generate a random unsigned 32-bit integer in the range `0..4294967295`:

    %% erlang
    RandomInetger = atomvm:random().

Use `atomvm:random_bytes/1` to return a randomly populated binary of a specified size:

    %% erlang
    RandomBinary = erlang:random_bytes(32).

Use `base64:encode/1` and `base64:decode/1` to encode to and decode from Base64 format.  The input value to these functions may be a binary or string.  The output value from these functions is an Erlang binary.

    %% erlang
    Encoded = base64:encode(<<"foo">>).
    <<"foo">> = base64:decode(Encoded).

You can Use `base64:encode_to_string/1` and `base64:decode_to_string/1` to perform the same encoding, but to return values as Erlang list structures, instead of as binaries.

## ESP32-specific APIs

Certain APIs are specific to and only supported on the ESP32 platform.  This section describes these APIs.

### System-Level APIs

As noted above, the `erlang:system_info/1` function can be used to obtain system-specific information about the platform on which your application is deployed.

You can request ESP32-specific information using using the following input atoms:

* `esp_free_heap_size` Returns the available free space in the ESP32 heap
* `esp_chip_info` Returns 4-tuple of the form `{esp32, Features, Cores, Revision}`, where `Features` is a bit mask of features enabled in the chip, `Cores` is the number of CPU cores on the chip, and `Revision` is the chip version.
* `esp_idf_version` Return the IDF SDK version, as a string.

For example,

    %% erlang
    FreeHeapSize = erlang:system_info(esp_free_heap_size).

### Non-volatile Storage

AtomVM provides functions for setting, retrieving, and deleting key-value data in binary form in non-volatile storage (NVS) on an ESP device.  Entries in NVS survive reboots of the ESP device, and can be used a limited "persistent store" for key-value data.

> Note.  NVS storage is limited in size, and NVS keys are restricted to 15 characters.

NVS entries are stored under a namespace and key, both of which are expressed as atoms.  AtomVM uses the namespace `atomvm` for entries under its control.  Applications may read from and write to the `atomvm` namespace, but they are strongly discouraged from doing so.

To set a value in non-volatile storage, use the `esp:set_binary/3` function, and specify a namespace, key, and value:

    %% erlang
    Namespace = <<"my-namespace">>,
    Key = <<"my-key">>,
    esp:set_binary(Namespace, Key, <<"some-value">>).

To retrieve a value in non-volatile storage, use the `esp:get_binary/2` function, and specify a namespace and key.  You can optionally specify a default value, if an entry does not exist in non-volatile storage:

    %% erlang
    Value = esp:get_binary(Namespace, Key, <<"default-value">>).

To delete an entry, use the `esp:erase_key/2` function, and specify a namespace and key:

    %% erlang
    ok = esp:erase_key(Namespace, Key).

You can delete all entries in a namespace via the `esp:erase_all/1` function:

    %% erlang
    ok = esp:erase_all(Namespace).

Finally, you can delete all entries in all namespaces on the NVS partition via the `esp:reformat/0` function:

    %% erlang
    ok = esp:reformat().

Applications should use the `esp:reformat/0` function with caution, in case other applications are making using the non-volatile storage.

> Note.  NVS entries are currently stored in plaintext and are not encrypted.  Applications should exercise caution if sensitive security information, such as account passwords, are stored in NVS storage.

### Restart and Deep Sleep

You can use the `esp:restart/0` function to immediately restart the ESP32 device.  This function does not return a value.

    %% erlang
    esp:restart().

Use the `esp:reset_reason/0` function to obtain the reason for the ESP32 restart.  Possible values include:

* `esp_rst_unknown`
* `esp_rst_poweron`
* `esp_rst_ext`
* `esp_rst_sw`
* `esp_rst_panic`
* `esp_rst_int_wdt`
* `esp_rst_task_wdt`
* `esp_rst_wdt`
* `esp_rst_deepsleep`
* `esp_rst_brownout`
* `esp_rst_sdio`

Use the `esp:deep_sleep/1` function to put the ESP device into deep sleep for a specified number of milliseconds.  Be sure to safely shut down any critical processes running before this function is called, as it will cause an immediate shutdown of the device.

    %% erlang
    esp:deep_sleep(60*1000).

Use the `esp:sleep_get_wakeup_cause/0` function can be used to inspect the reason for a wakeup.  Currently, the only supported return value is the atom `undefined` or `sleep_wakeup_timer`.

    %% erlang
    case esp:sleep_get_wakeup_cause() of
        sleep_wakeup_timer ->
            io:format("Woke up from a timer~n");
        _ ->
            io:format("Woke up for some other reason~n")
    end.

### RTC Memory

TODO -- needs to be merged in from atomvm_lib

## Peripherals

The AtomVM virtual machine and libraries support APIs for interfacing with peripheral devices connected to the ESP32.  This section provides information about these APIs.

### GPIO

You can read and write digital values on GPIO pins using the `gpio` module, using the `digital_read/1` and `digital_write/2` functions.  You must first set the direction of the pin using the `gpio:set_direction/2` function, using `input` or `output` as the direction parameter.

To read the value of a GPIO pin (`high` or `low`), use `gpio:digital_read/1`:

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, input),
    case gpio:digital_read(Pin) of
        high ->
            io:format("Pin ~p is high ~n", [Pin]);
        low ->
            io:format("Pin ~p is low ~n", [Pin])
    end.

To set the value of a GPIO pin (`high` or `low`), use `gpio:digital_write/2`:

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, output),
    gpio:digital_write(Pin, low).

#### Interrupt Handling

You can get notified of changes in the state of a GPIO pin by using the `gpio:set_int/2` function.  This function takes a reference to a GPIO Pin and a trigger.  Allowable triggers are `rising`, `falling`, `both`, `low`, `high`, and `none` (to disable an interrupt).

When a trigger event occurs, such as a pin rising in voltage, a tuple will be delivered to the process containing the trigger and the pin.

    %% erlang
    Pin = 2,
    gpio:set_direction(Pin, input),
    GPIO = gpio:open(),
    ok = gpio:set_int(GPIO, Pin, rising),
    receive
        {rising, Pin} ->
            io:format("Pin ~p is rising ~n", [Pin])
    end.

Interrupts can be removed by using the `gpio:remove_int/2` function.

### LED Control

The LED Control API can be used to drive LEDs, as well as generate PWM signals on GPIO pins.

The LEDC API is encapsulated in the `ledc` module and is a direct translation of the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a>, with a natural mapping into Erlang.  This API is intended for users with complex use-cases, and who require low-level access to the LEDC APIs.

The `ledc.hrl` module should be used for common modes, channels, duty cycle resolutions, and so forth.

    %% erlang
    -include("ledc.hrl").

    ...

    %% create a 5khz timer
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    Channel = ?LEDC_CHANNEL_0,
    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]).

    %% bind pin 2 to this timer in a channel
    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, 2},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]).

    %% set the duty cycle to 0, and fade up to 16000 over 5 seconds
    ledc:set_duty(SpeedMode, Channel, 0).
    ledc:update_duty(SpeedMode, Channel).
    TargetDuty = 16000.
    FadeMs = 5000.
    ok = ledc:set_fade_with_time(SpeedMode, Channel, TargetDuty, FadeMs).


### I2C

### SPI

### ADC

TODO -- needs to be merged in from atomvm_adc

### UART


## Protocols

### Network

### TCP

### UDP
