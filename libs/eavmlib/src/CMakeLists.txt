##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(eavmlib)

include(BuildErlang)

set(ERLANG_MODULES
    atomvm
    avm_logger
    avm_util
    console
    esp
    dht
    gpio
    httpd
    api_handler
    i2c
    http_server
    json_encoder
    ledc
    ledc_pwm
    network_fsm
    spi
    system_info_api_handler
    timer_manager
    timestamp_util
    uart
)

pack_archive(eavmlib ${ERLANG_MODULES})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/eavmlib.avm DESTINATION lib/AtomVM/ebin)
