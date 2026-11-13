#!/bin/bash
# Starts the modded game for the bot with its own home folder, so that bot
# runs never write to the player's real Balatro save.
# First use: copy the Mods folder and a profile into
#   $BOT_HOME/Library/Application Support/Balatro
set -euo pipefail

BOT_HOME="${BALATRO_BOT_HOME:-$HOME/.balatro-bot-home}"
GAME_DIR="${BALATRO_GAME_DIR:-$HOME/Library/Application Support/Steam/steamapps/common/Balatro}"
PORT="${BALATROBOT_PORT:-12346}"

if [ "$BOT_HOME" = "$HOME" ]; then
    echo "BALATRO_BOT_HOME must not be the real home folder: that would write the real save" >&2
    exit 1
fi

for mod in balatrobot smods; do
    if [ ! -d "$BOT_HOME/Library/Application Support/Balatro/Mods/$mod" ]; then
        echo "no $mod mod under $BOT_HOME/Library/Application Support/Balatro/Mods" >&2
        exit 1
    fi
done

cd "$GAME_DIR"
HOME="$BOT_HOME" CFFIXED_USER_HOME="$BOT_HOME" DYLD_INSERT_LIBRARIES=liblovely.dylib \
    BALATROBOT_HOST=127.0.0.1 BALATROBOT_PORT="$PORT" \
    exec ./Balatro.app/Contents/MacOS/love "$@"
