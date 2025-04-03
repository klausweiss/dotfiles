if command -v gpgconf &> /dev/null
	set -x GPG_TTY (tty)
	set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
	gpgconf --launch gpg-agent
else if test -n "$DESKTOP_SESSION"
	set -x SSH_AUTH_SOCK (gnome-keyring-daemon --start ^/dev/null | grep SSH_AUTH_SOCK | awk -F= '{print $2}')
end
