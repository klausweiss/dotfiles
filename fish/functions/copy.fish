switch (uname)
case Linux
    function copy
        command xsel -ib
    end
case Darwin
    function copy
        command pbcopy
    end
end
