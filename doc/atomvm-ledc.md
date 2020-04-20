# LED Control API

The LED Control API can be used to drive LEDs, as well as generate PWM signals on GPIO pins.

The LED Control API is implemented using the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a> and is currently only available on the ESP32 platform.

Two APIs are provided:

* A low-level API, which is a direct mapping onto the IDF SDK;
* A high-level API, which provides a highly simplified version of the low-level API and which is less error-prone, and which provides no loss of generality for most use-cases.

This page describes both APIs below.

## Low-level API

The low-level API is encapsulated in the `ledc` module and is a direct translation of the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a>, with a natural mapping into Erlang.  This API is intended for users with complex use-cases, and who require low-level access to the LEDC APIs.

The details of this API are best understood by reading the relevant IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a> documentation, which provides sufficient detail of the C API, of which the low-level API is a direct mapping.

> Note that some of the less common C APIs are not currently supported, but could be added with sufficient ease.  More challenging, but interesting, would be implemeting interrupts for fade operations.  Currently, applications are not notified when a fade operation completes.

The `ledc.hrl` module should be used for common modes, channels, duty cycle resolutions, and so forth.

### Sample code

The following sample code illustrates use of this API:

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

### Example Program

The `ledc_example.erl` example program is a functionally equivalent translation of the IDF SDK <a href="https://github.com/espressif/esp-idf/tree/master/examples/peripherals/ledc">LEDC Example Program</a>.

## High-level API

The high-level APIs is encapsulated in the `ledc_pwm` module and provide abstractions and state management around the low-level APIs, and helps to mitigate common errors with use of the low-level APIs.

The API is more "object-oriented", in that it separates the notion of a timer and channel into separate referencible entities, making it easier to manage the lifecycle of these objects.

Furthermore, the high-level API manages the relationship between a frequency set on a timer, the duty cycle resolution defined by that frequency, and the duty cycle, as set by the user.  As the details between this values is hidden, the user can set a frequency for a timer, and then specify the duty cycle as a percentage (a value between 0 and 100, inclusive), so that he or she does not need to manually compute which duty cycles are appropriate for which frequency, as one needs to do in the low-level API.

### Sampel Code

The following code illustrates use of the high-level API:

    %% create a 5khz timer
    Freq = 5000.
    {ok, Timer} = ledc_pwm:create_timer(Freq).

    %% bind pin 2 to this timer in a channel
    Pin = 2.
    {ok, Channel} = ledc_pwm:create_channel(Timer, Pin).

    io:format("Frequency(hz): ~p Duty(%): ~p~n", [ledc_pwm:get_freq_hz(Channel), ledc_pwm:get_dutyp(Channel)]).

    %% set the duty cycle to 0%, and fade up to 100% over 5 seconds
    TargetDutyp = 100.
    FadeMs = 5000.
    ledc_pwm:set_dutyp(Channel, 0).
    ledc_pwm:fade(Channel, TargetDutyp, FadeMs).

### Example Program

The 'ledc_pwm_example` program illustrates use of the high-level API by fading two LEDs, one automatically smoothly using the built-in fade functions, and on manually and step-wise, by setting the duty cycle percentage explicitly in code.
