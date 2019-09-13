function wallpaper
    set f $argv[1]
    mkdir -p ~/doc/images/wallpapers/
    cp $f ~/doc/images/wallpapers/
    and ln -sf ~/doc/images/wallpapers/(basename $f) ~/.config/wallpaper 
end
