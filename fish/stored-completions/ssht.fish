# Completion for the ssht function, mimicking ssh completions

# Get hostnames from known_hosts
function __fish_known_hosts
    cat ~/.ssh/known_hosts 2>/dev/null | \
        cut -f 1 -d ' ' | \
        sed 's/,/\n/g' | \
        grep -vE '^\[.*\]$' | \
        sort -u
end

# Get hostnames from ssh config
function __fish_ssh_config_hosts
    grep -i "^Host " ~/.ssh/config 2>/dev/null | \
        sed 's/^[Hh]ost //' | \
        tr ' ' '\n' | \
        grep -v '[*?]' | \
        sort -u
end

# Complete hostnames
complete -c ssht -f -a "(__fish_known_hosts; __fish_ssh_config_hosts)" -d "SSH host"

