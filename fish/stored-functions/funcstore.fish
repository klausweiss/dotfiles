function funcstore
set fname $argv[1]
mv ~/.config/fish/functions/$fname.fish ~/.config/fish/stored-functions/
echo "funcstore: moved ~/.config/fish/functions/$fname.fish to ~/.config/fish/stored-functions/$fname.fish"
end
