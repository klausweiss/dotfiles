#!/bin/bash

DOTFILES_DIR=$(realpath `dirname $0`/../../)
TEMPLATE_FILE=$DOTFILES_DIR/git/gitconfig
DESIRED_PATH=$HOME/.gitconfig

setup-git() {
    echo "Enter your full name (for git identifaction):"
    read USER_FULLNAME
    echo "Enter your email address (for git identification):"
    read USER_EMAIL
    cat $TEMPLATE_FILE |
        sed "/name =\W*$/  s/name.*$/name = $USER_FULLNAME/" |
        sed "/email =\W*$/ s/email.*$/email = $USER_EMAIL/" > $DESIRED_PATH
}

GIT_FILLED_FIELDS=$(test -f $HOME/.gitconfig && cat $HOME/.gitconfig | grep -e "\(name\|email\) = \w\+" | wc -l || echo 0)
if [ $GIT_FILLED_FIELDS = 2 ]; then
    echo "git already set up";
else
    setup-git;
fi
