#!/usr/bin/env bash

debug() {
    debug_on
}

debug_on() {
    if [ -n "$DEBUG" ]; then
        # set -v
        set -x
    fi
}

debug_off() {
    if [ -n "$DEBUG" ]; then
        set +x
        # set +v
    fi
}
