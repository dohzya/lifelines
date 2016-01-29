#!/bin/bash

assets="assets"
assets_js="$assets/js"
assets_sass="$assets/sass"

public="public"
public_js="$public/js"
public_css="$public/css"

./node_modules/.bin/node-sass "$assets_sass/main.scss" -o "$public_css/" -w &
sass_pid="$?"

babel_opts="[ babelify --presets [ es2015 ] ]"
# shellcheck disable=SC2086
./node_modules/.bin/watchify -t $babel_opts "$assets_js/index.js" -o "$public_js/index.js" || exit 3

kill "$sass_pid" &>/dev/null
sleep 1 &>/dev/null
kill -9 "$sass_pid" &>/dev/null
