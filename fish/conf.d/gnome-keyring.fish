if test -n "$DESKTOP_SESSION"
  set -x SSH_AUTH_SOCK (gnome-keyring-daemon --start ^/dev/null | grep SSH_AUTH_SOCK | awk -F= '{print $2}')
end
