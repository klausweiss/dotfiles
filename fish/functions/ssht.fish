# Defined in /tmp/fish.7hb7JL/ssht.fish @ line 2
function ssht
	ssh -t $argv "env SHELL=`which fish` tmux new-session -A -s mbiel"
end
