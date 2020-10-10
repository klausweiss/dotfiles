switch (uname)
case Linux
    function copy
        command perl -0 -pe 's/\n\Z//' | xsel -ib
    end
case Darwin
    function copy
        command pbcopy
    end
end
