function find-agent() {
    for f in /tmp/ssh-*/agent* ; do
        if [ -r "$f" ]; then
            echo $f
            return
        fi
    done
}

function fixssh() {
    source ~/.fixssh
    if [ ! -S "$SSH_AUTH_SOCK" ]; then
        agent=`find-agent 2>/dev/null`
        echo "SSH_AUTH_SOCK=$agent; export SSH_AUTH_SOCK" > ~/.fixssh
        source ~/.fixssh
    fi
}


if [ -S "$SSH_AUTH_SOCK" ]; then
    echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK; export SSH_AUTH_SOCK" > ~/.fixssh
else
    fixssh
fi
