source /etc/profile
source ~/.profile

source virtualenvwrapper_lazy.sh

export GOOGLE=8.8.8.8

export A=~/arc/
export ext=$A/library/http/fetch/exthttpcodes.h

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
typeset -A ZSH_HIGHLIGHT_STYLES

if [ "$TTY" = "linux" ]; then
    export LC_ALL=C;
fi

ZSH_HIGHLIGHT_STYLES=(
        'alias'           'fg=green'
        'builtin'         'fg=cyan'
        'function'        'fg=red'
        'command'         'fg=255,bold'
        'precommand'      'fg=magenta, underline'
        'hashed-commands' 'fg=cyan'
        'path'            'underline'
        'globbing'        'fg=166'
)

#export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on"
READNULLCMD=less

# insert-square-brackets() {
#  LBUFFER="${LBUFFER}["
#  RBUFFER="]${RBUFFER}"
#}
#zle -N insert-square-brackets
#bindkey "[" insert-square-brackets
#
#insert-brackets() {
#  LBUFFER="${LBUFFER}("
#  RBUFFER=")${RBUFFER}"
#}
#zle -N insert-brackets
#bindkey "(" insert-brackets

NOTIFY_ICON="terminal"
NOTIFY_COMMAND_TIMEOUT=30
export _JAVA_AWT_WM_NONREPARENTING=1

#CLAWS='claws-mail --status | sed -e "s/^[0-9]*\ \([0-9]*\).*/\1/g" | sed -e "s/^$/0/g"'
#[ ! "$UID" = "0" ] && [ $(bash -c $CLAWS) != 0 ] && echo "===========You have unread mail===========";


alias ls='ls --color=auto'


export EDITOR=vi

if [ -f "$HOME/.zsh/default-keybinds" ]; then
    source "$HOME/.zsh/default-keybinds";
fi

if [ -f "$HOME/.zsh/$TERM-keybinds" ]; then
    source "$HOME/.zsh/$TERM-keybinds";
fi

function bind-key() {
    $EDITOR $HOME/.zsh/$TERM-keybinds
    exec zsh;
}

setopt prompt_subst
autoload -U promptinit
setopt shwordsplit
promptinit

#[ ! "$UID" = "0" ] && PROMPT='%B%F{blue}%n@%m%f%F{blue}%f%b%(!.#.$) '

function zshexit() {
    clear
}

function get_hg_branch() {
    max_depth=${#${PWD//[^\/]}}
    i=1
    cur_dir=$PWD
    while (( $i <= $max_depth )); do
        if [ ! -w $cur_dir ]; then
            return 1;
        fi
        branch_file="$cur_dir/.hg/branch"
        if [ -f $branch_file ]; then
            cat $branch_file;
            return 0;
        fi
        cur_dir="$cur_dir/.."
        ((i++))
    done
    return 1;
}

function with_cvs() {
    local PWD_STYLE=$1
    svn_url=`svn info . 2>/dev/null | grep Relative | cut -d ':' -f3`
    if [ "x$svn_url" != "x" ]; then
        echo -n "%F{red}svn%f on%F{yellow}%B$svn_url%b%f";
        return 0;
    fi
    git_branch=`git symbolic-ref HEAD 2>/dev/null | sed -e "s/refs\/heads\///"`
    if [ "x$git_branch" != "x" ]; then
        echo -n "%F{red}git%f on %F{green}$git_branch%f : $PWD_STYLE";
        return 0;
    fi
    hg_branch=`get_hg_branch 2>/dev/null`
    if [ "x$hg_branch" != "x" ]; then
        echo -n "%F{red}hg%f on %F{magenta}$hg_branch%f : $PWD_STYLE";
        return 0;
    fi
    echo -n "$PWD_STYLE"
}

[ ! "$UID" = "0" ] && PROMPT='($(cvs_prompt))%B%F{blue}%2~%f%F{blue}%f%b> '
[  "$UID" = "0" ] && PROMPT='($(cvs_prompt))%B%F{red}%2~%f%F{blue}%f%b> '
RPROMPT="%{$fg_bold[grey]%}(%*)%{$reset_color%}%"

get_visible_length() {
    echo `echo $1 | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" | wc -m`
}

pre-prompt() {
  local PREPROMPT="%F{yellow}%m%f%F{blue}/%f"
  local PWD_STYLE="%B%F{blue}%2~%b%f"
  [  "$UID" = "0" ] && PWD_STYLE="%B%F{red}%2~%b%f"
  local WITH_CVS=`with_cvs "$PWD_STYLE"`
  local LEFT="%F{black}%B.%b%f%B%F{green}(%f%b$PREPROMPT$WITH_CVS%B%F{green})%b"
  if [ ! -z $VIRTUAL_ENV ]; then
    LEFT="$LEFT%F{red}[`echo $VIRTUAL_ENV | rev | cut -d'/' -f1 | rev`]%f"
  fi
  # -- color
  LEFT="$LEFT%F{black}%B"
  local RIGHT="."
  #"%F{green}(%f%F{grey}%n%f%F{green})%f%F{black}%B.%f%b"
  LEFT_P="$(print -P "$LEFT")"
  local RIGHT_P="$(print -P "$RIGHT")"
  local LEFTWIDTH=`get_visible_length "$LEFT_P"`
  local RIGHT_DELTA=$(($#RIGHT_P-`get_visible_length $RIGHT_P`))
  local RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH))
  if [ $RIGHTWIDTH -lt 1 ]; then
    PWD_STYLE="%B%F{blue}%1~%b%f"
    [  "$UID" = "0" ] && PWD_STYLE="%B%F{red}%1~%b%f"
    LEFT="%F{black}%B.%b%f%B%F{green}(%b$PREPROMPT$PWD_STYLE%B%F{green})%b"
    LEFT="$LEFT%F{black}%B"
    LEFT_P="$(print -P "$LEFT")"
    LEFTWIDTH=`get_visible_length "$LEFT_P"`
    RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH+$RIGHT_DELTA))
  fi
  LEFT_P="$(print -P "$LEFT")"
  RIGHT_P="$(print -P "$RIGHT")"
  LEFTWIDTH=`get_visible_length "$LEFT_P"`
  RIGHT_DELTA=$(($#RIGHT_P-`get_visible_length $RIGHT_P`))
  RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH))
  local GREETER="%F{white}%B>%b%f"
  [  "$UID" = "0" ] && GREETER="%F{red}%B>%b%f"
  if [ $RIGHTWIDTH -lt 1 ]; then
    RPROMPT=""
    PROMPT='%F{black}%B-%b%f'"$GREETER "
  else
    PROMPT="$LEFT${(l:$RIGHTWIDTH::-:)RIGHT}"'
%F{black}%B\`--%b%f'"$GREETER "
    RPROMPT="%F{grey}%B(%*)%b%f"
  fi
}


add-zsh-hook precmd pre-prompt

chprompt() {
  RPROMPT=""
  PROMPT="%B%F{blue}%2~%f%F{blue}%f%b> "
}

# -[ completion ]-
autoload -Uz compinit
compinit


function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip

#autoload predict-on
#predict-on

#zstyle ':completion:*' menu yes select
#zstyle ':completion:*' use-compctl false
zstyle :predict verbose yes
zstyle :predict cursor key
zstyle ':completion:predict:*' completer _oldlist _complete _ignored _history _prefix
zstyle ':completion:*' completer _complete _match #_approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' verbose true

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
bindkey '^E' autosuggest-accept

zstyle ':completion:*:processes' menu yes select
zstyle ':completion:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -xuf'
zstyle ':completion:*:processes' sort false
zstyle ':completion:*:processes-names' command 'ps xho command'

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

# -[ history ]-
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt append_history hist_ignore_all_dups hist_ignore_space autocd extendedglob autopushd
export LS_COLORS='*.py=3'

# -[ functions ]-
name() {
    name=$1
    vared -c -p 'rename to: ' name
    command mv $1 $name
}

mkd() { mkdir $1; cd $1 }

makeproject() {
    if [ "$#" -ne 2 ]; then
        echo "Wrong usage"
        return 1
    fi
    mkdir $2
    cd $2
    cp ~/Documents/templates/$1/* . -r
    make edit
}

alias mkproject="makeproject c++ "
alias mkjproject="makeproject java "
alias mkpproject="makeproject python "

alias rmrf="rm -rf $1"

pk () {
 if [ $1 ] ; then
 case $1 in
 tbz)       tar cjvf $2.tar.bz2 $2      ;;
 tgz)       tar czvf $2.tar.gz  $2       ;;
 tar)      tar cpvf $2.tar  $2       ;;
 bz2)    bzip $2 ;;
 gz)        gzip -c -9 -n $2 > $2.gz ;;
 zip)       zip -r $2.zip $2   ;;
 7z)        7z a $2.7z $2    ;;
 *)         echo "'$1' cannot be packed via pk()" ;;
 esac
 else
 echo "'$1' is not a valid file"
 fi

}

extract () {
 if [ -f $1 ] ; then
 case $1 in
 *.tar.bz2)   tar xjf $1        ;;
 *.tar.gz)    tar xzf $1     ;;
 *.bz2)       bunzip2 $1       ;;
 *.rar)       unrar x $1     ;;
 *.gz)        gunzip $1     ;;
 *.tar)       tar xf $1        ;;
 *.tbz2)      tar xjf $1      ;;
 *.tbz)       tar -xjvf $1    ;;
 *.tgz)       tar xzf $1       ;;
 *.zip)       unzip $1     ;;
 *.Z)         uncompress $1  ;;
 *.7z)        7z x $1    ;;
 *)           echo "I don't know how to extract '$1'..." ;;
 esac
 else
 echo "'$1' is not a valid file"
 fi
}

blacklist_regexp="^\(bpython|gdb|mc|livestreamer|okular|zathura|tmux|less|nano|vim|vi|mutt|man|qvim|gdb|fbless|htop\).*"

function store-command-stats() {
  last_command=$1
  last_command_name=${1[(wr)^(*=*|sudo|ssh|-*)]}
  start_time=`date "+%s"`
}

function notify-error {
  local now diff start_time last_command

  start_time=$1
  last_command="$2"
  now=$(date "+%s")
  (( diff = $now - $start_time ))
  if (( $diff > $NOTIFY_COMMAND_TIMEOUT )); then
    notify-send -u critical -i $NOTIFY_ICON "$2 failed";
  fi
}

function notify-success() {
  local now diff start_time last_command

  start_time=$1
  last_command="$2"
  now=$(date "+%s")
  (( diff = $now - $start_time ))
  if (( $diff > $NOTIFY_COMMAND_TIMEOUT )); then
    notify-send -u normal -i $NOTIFY_ICON "$2 finished";
  fi
}

function notify-command-complete() {
  last_status=$?
  if ! [[ $last_command =~ $blacklist_regexp ]]; then
    if [[ $last_status -gt "0" ]]; then
      notify-error "$start_time" "$last_command" 2>/dev/null
    elif [[ -n $start_time ]]; then
      notify-success "$start_time" "$last_command" 2>/dev/null
    fi
  fi
  unset last_command start_time last_status
}

alias op="vblank_mode=0 primusrun "

add-zsh-hook preexec store-command-stats
add-zsh-hook precmd notify-command-complete

# -[ alias ]-
#alias -s avi=vlc --fbdev=/dev/fb0
alias -s proto="$EDITOR"
alias -s jar=java -jar
alias -s fb2=fbless
alias -s cpp="$EDITOR"
alias -s h="$EDITOR"
alias -s pdf="zathura"
alias -s djvu="zathura"
alias -s hs=runhaskell
alias -s txt="$EDITOR"
#alias -s mkv=vlc --fbdev=/dev/fb0
#alias -s mp4=vlc --fbdev=/dev/fb0
#alias -s mov=vlc --fbdev=/dev/fb0
alias -s avi=vlc
alias -s mkv=vlc
alias -s mp4=vlc
alias -s mov=vlc
alias -s exe=wine
alias -s EXE=wine
alias -s png=gwenview
alias -s vim="vim -S "

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^Z" insert-sudo

# URL encode something and print it.
function url-encode; {
        setopt extendedglob
        echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

# Search google for the given keywords.
function google; {
        $BROWSER "http://www.google.com/search?q=`url-encode "${(j: :)@}"`"
}

function debug-flags; {
    echo -Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fsanitize=undefined -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2
}

function docker-clean() {
  docker ps -a | awk '{print $1}' | xargs --no-run-if-empty docker rm
  docker rmi $(docker images -f dangling=true -q)
}

function get-arcadia() {
    local DIR=$1
    "svn" cat svn+ssh://arcadia.yandex.ru/arc/trunk/arcadia/ya | python - clone $DIR
    echo 'PATH=$PATH:'$DIR >> ~/.profile
    $DIR/ya completion --zsh
    chmod 755 -R ~/.ya.completion
    exec zsh
}

function set-title() {
    echo -ne "\033]0;$@\007"
}

function totp {
    eval `gpg -d ~/.secrets.gpg`
    eval oathtool --totp \$$1 --base32
}

#alias hg="ya tool hg"
if which rg >/dev/null; then
    alias ag="rg";
fi
alias gst="git status"
alias lein-repl="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m clojure.main"
alias popd="popd -q"
alias ls='ls --classify --color --human-readable --group-directories-first'
alias battery="acpi -b | sed -e 's/.* \([0-9]*\)%.*$/\1/g'"
alias printFile="gtklp"
alias akos-proxy="ssh -D 5222 akos -N"
alias pasteit='pastebinit -b "http://slexy.org"'
alias gateway='ip route | grep default | cut -d" " -f3'
alias grep="grep --color -i -n "
alias yvim="ya vim"
alias ygdb="ya tool gdb"
alias ymake="ya make -j4"
alias valgrind="ya tool valgrind"
alias json="python -m json.tool"
alias st="svn status -q ~/arc-svn/"
alias -g yhg="ya tool hg"
alias hg-patch='yhg diff -r . -r `hg debugancestor . default`'

alias mosh="LC_ALL=en_US.UTF-8 mosh --server='~/bin/mosh-server'"
alias less="less -r"

source ~/.ya.completion/zsh/ya # YA_COMPLETION NAME='ya'

# Allow SSH tab completion for mosh hostnames
compdef mosh=ssh
# Allow mercurial completion for arcadia hg
compdef yhg=hg
