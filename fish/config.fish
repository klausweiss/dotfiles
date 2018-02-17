set fish_greeting ""

for file in ~/.config/fish/conf.d/*.fish
    source $file
end
