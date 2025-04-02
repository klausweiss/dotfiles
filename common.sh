#!/bin/bash

# Common functions useful in other parts of the config.

# Define colors
GREEN="\e[32m"
YELLOW="\e[33m"
RESET="\e[0m"

# Function to get the absolute path of the script directory
get_script_dir() {
  cd "$( dirname "${BASH_SOURCE[1]}" )" &> /dev/null && pwd
}

# Function to safely create a symlink
ensure_symlink() {
  local target_file="$1"
  local target_link="$2"

  # Get the current date and time in the format YYYY-MM-DD_HH-MM-SS
  local datetime=$(date +"%Y-%m-%d_%H-%M-%S")

  # Check if the symlink already exists and points to the correct target
  if [ -L "$target_link" ] && [ "$(readlink -f "$target_link")" == "$target_file" ]; then
    echo -e "${GREEN}Symlink for $target_link is already correctly set. Exiting.${RESET}"
    return 0
  fi

  # Backup the existing file or incorrect symlink if it exists
  if [ -e "$target_link" ] || [ -L "$target_link" ]; then
    mv "$target_link" "$target_link.bak.$datetime"
    echo -e "${YELLOW}Existing file/symlink $target_link moved to $target_link.bak.$datetime${RESET}"
  fi

  # Create the correct symlink
  ln -s "$target_file" "$target_link"
  echo -e "${GREEN}Symlink created: $target_link → $target_file${RESET}"
}

