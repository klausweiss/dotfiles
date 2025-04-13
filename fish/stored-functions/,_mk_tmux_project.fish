function ,_mk_tmux_project
    set project_path (realpath $argv[1])
    set name (path basename $project_path)
    set fname ",tmux-$name"

    # Create function body with expanded variables
    set -l body ",_tmux_project $name $project_path"

    # Define the function with the body evaluated at this moment
    eval "
        function $fname
            $body
        end
    "

    funcsave $fname
end
