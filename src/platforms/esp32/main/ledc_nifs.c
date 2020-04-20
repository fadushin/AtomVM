/***************************************************************************
 *   Copyright 2020 by Fred Dushin <fred@dushin.net>                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#define _GNU_SOURCE

#include "atom.h"
#include "defaultatoms.h"
#include "platform_defaultatoms.h"
#include "nifs.h"
#include "memory.h"
#include "term.h"
#include "interop.h"

#include <stdlib.h>
#include <driver/ledc.h>

//#define ENABLE_TRACE
#include "trace.h"

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = ERROR_ATOM; \
        argv[1] = BADARG_ATOM; \
        return term_invalid_term(); \
    }

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM; \
    ctx->x[1] = (error_type_atom); \
    return term_invalid_term();

static const char *const ledc_duty_resolution   = "\xF"  "duty_resolution";
static const char *const ledc_freq_hz           = "\x7"  "freq_hz";
static const char *const ledc_speed_mode        = "\xA"  "speed_mode";
static const char *const ledc_timer_num         = "\x9"  "timer_num";
static const char *const ledc_channel           = "\x7"  "channel";
static const char *const ledc_duty              = "\x4"  "duty";
static const char *const ledc_gpio_num          = "\x8"  "gpio_num";
static const char *const ledc_hpoint            = "\x6"  "hpoint";
static const char *const ledc_timer_sel         = "\x9"  "timer_sel";
//                                                        123456789ABCDEF01


static term nif_ledc_timer_config(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term config = argv[0];
    VALIDATE_VALUE(config, term_is_list);

    term speed_mode = interop_proplist_get_value(config, context_make_atom(ctx, ledc_speed_mode));
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term duty_resolution = interop_proplist_get_value(config, context_make_atom(ctx, ledc_duty_resolution));
    VALIDATE_VALUE(duty_resolution, term_is_integer);
    term freq_hz = interop_proplist_get_value(config, context_make_atom(ctx, ledc_freq_hz));
    VALIDATE_VALUE(freq_hz, term_is_integer);
    term timer_num = interop_proplist_get_value(config, context_make_atom(ctx, ledc_timer_num));
    VALIDATE_VALUE(timer_num, term_is_integer);

    ledc_timer_config_t ledc_timer = {
        .duty_resolution = term_to_int(duty_resolution),
        .freq_hz = term_to_int(freq_hz),
        .speed_mode = term_to_int(speed_mode),
        .timer_num = term_to_int(timer_num)
    };
    esp_err_t err = ledc_timer_config(&ledc_timer);
    switch (err) {
        case ESP_OK:
            TRACE("ledc_timer_config\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to set ledc timer config. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_channel_config(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term config = argv[0];
    VALIDATE_VALUE(config, term_is_list);

    term gpio_num = interop_proplist_get_value(config, context_make_atom(ctx, ledc_gpio_num));
    VALIDATE_VALUE(gpio_num, term_is_integer);
    term speed_mode = interop_proplist_get_value(config, context_make_atom(ctx, ledc_speed_mode));
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = interop_proplist_get_value(config, context_make_atom(ctx, ledc_channel));
    VALIDATE_VALUE(channel, term_is_integer);
    term timer_sel = interop_proplist_get_value(config, context_make_atom(ctx, ledc_timer_sel));
    VALIDATE_VALUE(timer_sel, term_is_integer);
    term duty = interop_proplist_get_value(config, context_make_atom(ctx, ledc_duty));
    VALIDATE_VALUE(duty, term_is_integer);
    term hpoint = interop_proplist_get_value(config, context_make_atom(ctx, ledc_hpoint));
    VALIDATE_VALUE(hpoint, term_is_integer);

    ledc_channel_config_t ledc_channel = {
        .gpio_num = term_to_int(gpio_num),
        .speed_mode = term_to_int(speed_mode),
        .channel = term_to_int(channel),
        .timer_sel = term_to_int(timer_sel),
        .duty = term_to_int(duty),
        .hpoint = term_to_int(hpoint)
    };
    esp_err_t err = ledc_channel_config(&ledc_channel);
    switch (err) {
        case ESP_OK:
            TRACE("ledc_channel_config\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to set ledc channel config. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_fade_func_install(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term flags = argv[0];
    VALIDATE_VALUE(flags, term_is_integer);

    esp_err_t err = ledc_fade_func_install(term_to_int(flags));
    switch (err) {
        case ESP_OK:
            TRACE("ledc_fade_func_install\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to install fade func. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_fade_func_uninstall(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    ledc_fade_func_uninstall();
    TRACE("ledc_fade_func_install\n");
    return OK_ATOM;
}

static term nif_ledc_set_fade_with_time(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term fade_time = argv[3];
    VALIDATE_VALUE(fade_time, term_is_integer);

    esp_err_t err = ledc_set_fade_with_time(
        term_to_int(speed_mode),
        term_to_int(channel),
        term_to_int(duty),
        term_to_int(fade_time)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_set_fade_with_time\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_set_fade_with_time. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_set_fade_with_step(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);
    term scale = argv[3];
    VALIDATE_VALUE(scale, term_is_integer);
    term cycle_num = argv[3];
    VALIDATE_VALUE(cycle_num, term_is_integer);

    esp_err_t err = ledc_set_fade_with_step(
        term_to_int(speed_mode),
        term_to_int(channel),
        term_to_int(duty),
        term_to_int(scale),
        term_to_int(cycle_num)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_set_fade_with_step\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_set_fade_with_step. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_fade_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term fade_no_wait = argv[2];
    VALIDATE_VALUE(fade_no_wait, term_is_integer);

    esp_err_t err = ledc_fade_start(
        term_to_int(speed_mode),
        term_to_int(channel),
        term_to_int(fade_no_wait)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_fade_start\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_fade_start. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_get_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);

    uint32_t duty = ledc_get_duty(
        term_to_int(speed_mode),
        term_to_int(channel)
    );
    switch (duty) {
        case LEDC_ERR_DUTY:
            fprintf(stderr, "Unable to ledc_get_duty.\n");
            RAISE_ERROR(term_from_int(LEDC_ERR_DUTY));
        default:
            TRACE("ledc_get_duty\n");
            return term_from_int(duty);
    }
}

static term nif_ledc_set_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term duty = argv[2];
    VALIDATE_VALUE(duty, term_is_integer);

    esp_err_t err = ledc_set_duty(
        term_to_int(speed_mode),
        term_to_int(channel),
        term_to_int(duty)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_set_duty\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_set_duty. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_update_duty(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);

    esp_err_t err = ledc_update_duty(
        term_to_int(speed_mode),
        term_to_int(channel)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_update_duty\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_update_duty. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_get_freq(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term timer_num = argv[1];
    VALIDATE_VALUE(timer_num, term_is_integer);

    uint32_t freq_hz = ledc_get_freq(
        term_to_int(speed_mode),
        term_to_int(timer_num)
    );
    switch (freq_hz) {
        case 0:
            fprintf(stderr, "Unable to ledc_get_freq.\n");
            RAISE_ERROR(term_from_int(0));
        default:
            TRACE("ledc_get_freq\n");
            return term_from_int(freq_hz);
    }
}

static term nif_ledc_set_freq(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term timer_num = argv[1];
    VALIDATE_VALUE(timer_num, term_is_integer);
    term freq_hz = argv[2];
    VALIDATE_VALUE(freq_hz, term_is_integer);

    esp_err_t err = ledc_set_freq(
        term_to_int(speed_mode),
        term_to_int(timer_num),
        term_to_int(freq_hz)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_set_freq\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_set_freq. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_ledc_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term speed_mode = argv[0];
    VALIDATE_VALUE(speed_mode, term_is_integer);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_integer);
    term idle_level = argv[2];
    VALIDATE_VALUE(idle_level, term_is_integer);

    esp_err_t err = ledc_stop(
        term_to_int(speed_mode),
        term_to_int(channel),
        term_to_int(idle_level)
    );
    switch (err) {
        case ESP_OK:
            TRACE("ledc_stop\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to ledc_stop. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static const struct Nif ledc_timer_config_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_timer_config
};
static const struct Nif ledc_channel_config_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_channel_config
};
static const struct Nif ledc_fade_func_install_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_func_install
};
static const struct Nif ledc_fade_func_uninstall_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_func_uninstall
};
static const struct Nif ledc_set_fade_with_time_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_with_time
};
static const struct Nif ledc_set_fade_with_step_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_fade_with_step
};
static const struct Nif ledc_fade_start_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_fade_start
};
static const struct Nif ledc_get_duty_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_get_duty
};
static const struct Nif ledc_set_duty_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_duty
};
static const struct Nif ledc_update_duty_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_update_duty
};
static const struct Nif ledc_get_freq_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_get_freq
};
static const struct Nif ledc_set_freq_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_set_freq
};
static const struct Nif ledc_stop_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ledc_stop
};

const struct Nif *ledc_nifs_get_nif(const char *nifname)
{
    if (strcmp("ledc:timer_config/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_timer_config_nif;
    }
    if (strcmp("ledc:channel_config/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_channel_config_nif;
    }
    if (strcmp("ledc:fade_func_install/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_fade_func_install_nif;
    }
    if (strcmp("ledc:fade_func_uninstall/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_fade_func_uninstall_nif;
    }
    if (strcmp("ledc:set_fade_with_time/4", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_set_fade_with_time_nif;
    }
    if (strcmp("ledc:set_fade_with_step/5", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_set_fade_with_step_nif;
    }
    if (strcmp("ledc:fade_start/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_fade_start_nif;
    }
    if (strcmp("ledc:get_duty/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_get_duty_nif;
    }
    if (strcmp("ledc:set_duty/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_set_duty_nif;
    }
    if (strcmp("ledc:update_duty/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_update_duty_nif;
    }
    if (strcmp("ledc:get_freq/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_get_freq_nif;
    }
    if (strcmp("ledc:set_freq/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_set_freq_nif;
    }
    if (strcmp("ledc:stop/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &ledc_stop_nif;
    }
    return NULL;
}
