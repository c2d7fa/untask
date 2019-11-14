#!/usr/bin/env bash

# This file is a wrapper around the executable itself. It serves three purposes:
#
# 1. Since Racket is quite slow to start, we want to display an indicator that
#    something is happening when the user runs the executable.
#
# 2. If the user presses ctrl-c before the executable has fully loaded, Racket
#    will helpfully print out a detailed error message, even though no error
#    occurred. We get around this by just supressing all errors from the
#    executable. This is a total hack, but it's good enough for now.
#
# 3. Finally, since we haven't bothered implementing any kind of line editing
#    ourselves, we run the executable with 'rlwrap' to get rudimentary line
#    editing functionality.

# First line of the binary data in this file
BINARY_START_LINE=43

# Loading message
echo -e "\e[37mLoading Untask...\e[0m"

# We need to extract the executable to a temporary file in order to run it
executable_path="$(mktemp)"

# By default, Racket considers Ctrl-C an error, so we must make sure to succeed always; also, we need to delete the temporary file
trap "rm -f \"$executable_path\"; exit 0" EXIT

# Extract the executable
tail -n +$BINARY_START_LINE "$0" > "$executable_path"
chmod +x "$executable_path"

# Run the executable with line editing through 'rlwrap' and errors disabled
rlwrap "$executable_path" $@ 2>/dev/null

# Clean up
rm -f "$executable_path"

# We don't want to start interpreting binary data!
exit

## --- The following is the binary data --- ##
