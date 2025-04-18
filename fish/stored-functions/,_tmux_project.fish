function ,_tmux_project
set name $argv[1]
set project_path $argv[2]
tmux new-session -A -s $name -c $project_path
end
