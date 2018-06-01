#!/usr/bin/env bash

set -e

CONFIG_FILENAME="install.conf.yaml"

META_DIR=".meta"

DOTBOT_DIR="dotbot"
DOTBOT_BIN="bin/dotbot"

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

CONFIGS=${@}

for config in ${CONFIGS}; do
    echo -e "Configure $config"
    configContent="$(<"${BASE_DIR}/${config}/${CONFIG_FILENAME}")"
    "${BASE_DIR}/${META_DIR}/${DOTBOT_DIR}/${DOTBOT_BIN}" -d "${BASE_DIR}" -c <(echo -e "$configContent")
done
