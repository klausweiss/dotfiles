if status is-interactive
    # Commands to run in interactive sessions can go here
    set --prepend fish_function_path ~/.config/fish/stored-functions
    set --prepend fish_complete_path ~/.config/fish/stored-completions
end
