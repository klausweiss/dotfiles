#!/usr/bin/env bash

set -e

CONFIG_FILENAME="install.conf.yaml"

META_DIR=".meta"
PROFILES_DIR="profiles"

DOTBOT_DIR="dotbot"
DOTBOT_BIN="bin/dotbot"

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"


for PROFILE_FILE in ${@}; do
    while IFS= read -r config; do
        CONFIGS+=" ${config}"
    done < "${BASE_DIR}/${META_DIR}/${PROFILES_DIR}/$PROFILE_FILE"
    shift
done


for config in ${CONFIGS}; do
    echo -e "Configure $config"
    configContent="$(<"${BASE_DIR}/${config}/${CONFIG_FILENAME}")"
    "${BASE_DIR}/${META_DIR}/${DOTBOT_DIR}/${DOTBOT_BIN}" -q -d "${BASE_DIR}" -c <(echo -e "$configContent")
done
