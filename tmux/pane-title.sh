#!/bin/bash
# Fish-style abbreviated pwd ("~/.d/f/stored-functions") + git branch,
# pre-formatted with tmux #[] style tags for pane-border-format. Lives as a
# real script (rather than inline in tmux.conf) because tmux's config parser
# eats bare $VAR tokens and strftime %-specifiers before a #() shell command
# ever runs, which makes anything beyond trivial shell logic unworkable inline.

real_path="$1"
display_path="$real_path"

case "$display_path" in
	"$HOME") display_path="~" ;;
	"$HOME"/*) display_path="~${display_path#"$HOME"}" ;;
esac

IFS='/' read -ra parts <<< "$display_path"
last=$(( ${#parts[@]} - 1 ))
abbrev=""
for i in "${!parts[@]}"; do
	part="${parts[$i]}"
	if [ "$i" -eq "$last" ]; then
		piece="$part"
	elif [[ "$part" == .* ]]; then
		piece="${part:0:2}"
	else
		piece="${part:0:1}"
	fi
	if [ "$i" -eq 0 ]; then
		abbrev="$piece"
	else
		abbrev="$abbrev/$piece"
	fi
done

solid=$''

if branch=$(git -C "$real_path" symbolic-ref --short HEAD 2>/dev/null); then
	printf '%s' \
		"#[fg=colour252,bg=colour241] $abbrev " \
		"#[fg=colour241,bg=colour239]${solid}#[fg=colour223,bg=colour239] $branch " \
		"#[fg=colour239,bg=colour237]${solid}"
else
	printf '%s' \
		"#[fg=colour252,bg=colour241] $abbrev " \
		"#[fg=colour241,bg=colour237]${solid}"
fi
