#!/usr/bin/env bash

echo -e "\e[37mLoading Untask...\e[0m"
rlwrap racket /usr/local/lib/untask/main.rkt $@

