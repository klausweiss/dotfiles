function funcstore
    if test (count $argv) -ne 1
        echo "Usage: funcstore <function_name>"
        echo "Moves a fish function to the stored-functions directory."
        return 1
    end

    set fname $argv[1]
    set src_path ~/.config/fish/functions/$fname.fish
    set dest_path ~/.config/fish/stored-functions/$fname.fish

    if not test -f $src_path
        echo "Error: Function '$fname' does not exist in ~/.config/fish/functions/"
        return 1
    end

    mv $src_path $dest_path
    echo "funcstore: moved $src_path to $dest_path"
end

