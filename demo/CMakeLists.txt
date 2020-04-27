##
## Copyright (c) 2019 Fred Dushin <fred@dushin.net>
##

project(demo)

include(BuildErlang)

pack_runnable(hello hello eavmlib estdlib)
pack_runnable(pingpong pingpong eavmlib estdlib)
pack_runnable(blinky blinky eavmlib estdlib)
pack_runnable(dht_demo dht_demo eavmlib estdlib)
pack_runnable(wifi_demo wifi_demo eavmlib estdlib)
pack_runnable(httpd_demo httpd_demo eavmlib estdlib)
