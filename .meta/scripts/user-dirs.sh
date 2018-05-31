#!/bin/bash

do-if-exists() {
    COMMAND=$1
    ARGS="${@:2}"
    if [ -x "$(command -v $COMMAND)" ]; then
        $COMMAND $ARGS
    fi
}

DIRS="
    $HOME/doc/desktop
    $HOME/doc/docs
    $HOME/doc/images
    $HOME/doc/music
    $HOME/doc/shared
    $HOME/doc/videos
    $HOME/src
    $HOME/.templates
    $HOME/tmp
"
for DIR in $DIRS;
do
    mkdir -p $DIR
done

do-if-exists xdg-user-dirs-update
do-if-exists xdg-user-dirs-gtk-update
