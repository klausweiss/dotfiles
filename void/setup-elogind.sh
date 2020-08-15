#!/bin/bash

(sudo sv status elogind >/dev/null)
IS_SETUP=$?

if [ $IS_SETUP -eq 0 ]; then exit 0; fi

sudo rm -f /var/service/acpid
sudo ln -fs /etc/sv/elogind /var/service/
