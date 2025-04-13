function ssht
        ssh -t $argv "env SHELL=`which fish` TERM=xterm-256color tmux new-session -A -s mbiel"
end
