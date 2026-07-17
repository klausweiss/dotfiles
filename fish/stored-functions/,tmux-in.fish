function ,tmux-in
    set path_arg $argv[1]
    if test -z "$path_arg"
        set path_arg .
    end
    set project_path (realpath $path_arg)
    set name (path basename $project_path)
    ,_tmux_project $name $project_path
end
