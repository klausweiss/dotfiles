function __fish_complete_funcstore
    set -l func_dir ~/.config/fish/functions/
    for file in $func_dir*.fish
        echo (basename $file .fish)
    end
end

complete -c funcstore -f -a "(__fish_complete_funcstore)" -d "Move a function to stored-functions"

