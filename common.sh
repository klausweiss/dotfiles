#!/bin/bash

# Common functions useful in other parts of the config.

# Define colors
GREEN="\e[32m"
YELLOW="\e[33m"
RED="\e[31m"
RESET="\e[0m"

# Info log function
info() {
  local message="$@"
  echo -e "${GREEN}${message}${RESET}"
}

# Warn log function
warn() {
  local message="$@"
  echo -e "${YELLOW}${message}${RESET}"
}

# Error log function
error() {
  local message="$@"
  echo -e "${RED}${message}${RESET}"
}

# Function to get the absolute path of the script directory
# Only to be called outside common.sh
get_script_dir() {
  cd "$( dirname "${BASH_SOURCE[1]}" )" &> /dev/null && pwd
}

# Function to get the absolute path of the script directory
# Only to be called within common.sh
__common_sh_get_script_dir() {
  cd "$( dirname "${BASH_SOURCE[2]}" )" &> /dev/null && pwd
}

# Function to safely create a symlink (absolute version)
ensure_symlink_absolute() {
  local target_file="$1"
  local target_link="$2"

  # Get the current date and time in the format YYYY-MM-DD_HH-MM-SS
  local datetime=$(date +"%Y-%m-%d_%H-%M-%S")

  # Check if the symlink already exists and points to the correct target
  if [ -L "$target_link" ] && [ "$(readlink -f "$target_link")" == "$target_file" ]; then
    info "Symlink for $target_link is already correctly set. Exiting."
    return 0
  fi

  # Backup the existing file or incorrect symlink if it exists
  if [ -e "$target_link" ] || [ -L "$target_link" ]; then
    mv "$target_link" "$target_link.bak.$datetime"
    warn "Existing file/symlink $target_link moved to $target_link.bak.$datetime"
  fi

  # Create the correct symlink
  ln -s "$target_file" "$target_link"
  info "Symlink created: $target_link → $target_file"
}

# Wrapper function for ensure_symlink_absolute
ensure_symlink() {
  local target_file="$1"
  local target_link="$2"

  # Check if target_file is already an absolute path
  if [[ "$target_file" != /* ]]; then
    # If not, prepend the script directory
    local absolute_target_file=$(__common_sh_get_script_dir)/"$target_file"
  else
    # If it's already absolute, use it as-is
    local absolute_target_file="$target_file"
  fi
  absolute_target_file=$(realpath $absolute_target_file)

  # Call ensure_symlink_absolute with the absolute path of the target file
  ensure_symlink_absolute "$absolute_target_file" "$target_link"
}

