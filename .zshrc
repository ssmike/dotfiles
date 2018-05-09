source ~/.zprofile 2>/dev/null

source virtualenvwrapper_lazy.sh 2>/dev/null

export GOOGLE=8.8.8.8

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

typeset -A COLOR
COLOR=(
    "black"         0
    "br-black"      8
    "red"           1
    "br-red"        9
    "green"         2
    "br-green"      10
    "yellow"        3
    "br-yellow"     11
    "blue"          4
    "br-blue"       12
    "magenta"       5
    "br-magenta"    13
    "cyan"          6
    "br-cyan"       14
    "white"         7
    "br-white"      15
)

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
        echo -n "%F{$COLOR[red]}svn%f on%F{$COLOR[br-yellow]}$svn_url%f";
        return 0;
    fi
    git_branch=`git symbolic-ref HEAD 2>/dev/null | sed -e "s/refs\/heads\///"`
    if [ "x$git_branch" != "x" ]; then
        echo -n "%F{$COLOR[red]}git%f on %F{$COLOR[green]}$git_branch%f : $PWD_STYLE";
        return 0;
    fi
    hg_branch=`get_hg_branch 2>/dev/null`
    if [ "x$hg_branch" != "x" ]; then
        echo -n "%F{$COLOR[red]}hg%f on %F{$COLOR[magenta]}$hg_branch%f : $PWD_STYLE";
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
  local PREPROMPT="%F{$COLOR[yellow]}%m%f%F{$COLOR[blue]}/%f"
  local PWD_STYLE="%F{$COLOR[br-blue]}%2~%f"
  [  "$UID" = "0" ] && PWD_STYLE="%F{$COLOR[br-red]}%2~%f"
  local WITH_CVS=`with_cvs "$PWD_STYLE"`
  local LEFT="%F{$COLORS[br-black]}.%f%F{$COLOR[br-green]}(%f$PREPROMPT$WITH_CVS%F{$COLOR[br-green]})%f"
  if [ ! -z $VIRTUAL_ENV ]; then
    LEFT="$LEFT%F{$COLOR[red]}[`echo $VIRTUAL_ENV | rev | cut -d'/' -f1 | rev`]%f"
  fi
  # -- color
  LEFT="$LEFT%F{$COLOR[br-black]}"
  local RIGHT="."
  #"%F{green}(%f%F{grey}%n%f%F{green})%f%F{black}%B.%f%b"
  local LEFT_P="$(print -P "$LEFT")"
  local RIGHT_P="$(print -P "$RIGHT")"
  local LEFTWIDTH=`get_visible_length "$LEFT_P"`
  local RIGHT_DELTA=$(($#RIGHT_P-`get_visible_length $RIGHT_P`))
  local RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH))
  if [ $RIGHTWIDTH -lt 1 ]; then
    PWD_STYLE="%F{$COLOR[br-blue]}%1~%f"
    [  "$UID" = "0" ] && PWD_STYLE="%F{$COLOR[br-red]}%1~%f"
    LEFT="%F{$COLOR[br-black]}.%f%F{$COLOR[br-green]}($PREPROMPT$PWD_STYLE%F{$COLOR[br-green]})%f"
    LEFT="$LEFT%F{$COLOR[br-black]}"
    LEFT_P="$(print -P "$LEFT")"
    LEFTWIDTH=`get_visible_length "$LEFT_P"`
    RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH+$RIGHT_DELTA))
  fi
  LEFT_P="$(print -P "$LEFT")"
  RIGHT_P="$(print -P "$RIGHT")"
  LEFTWIDTH=`get_visible_length "$LEFT_P"`
  RIGHT_DELTA=$(($#RIGHT_P-`get_visible_length $RIGHT_P`))
  RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH))
  local GREETER="%F{$COLOR[br-white]}>%f"
  [  "$UID" = "0" ] && GREETER="%F{$COLOR[br-red]}>%f"
  if [ $RIGHTWIDTH -lt 1 ]; then
    RPROMPT=""
    PROMPT="%F{$COLOR[br-black]}-%f$GREETER "
  else
    PROMPT="$LEFT${(l:$RIGHTWIDTH::-:)RIGHT}"'
%F{'"$COLOR[br-black]"'}\`--%f'"$GREETER "
    RPROMPT="%F{$COLOR[br-white]}(%*)%f"
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

mypy-venv() {
    export MYPYPATH=`python -c "import sys; print(sys.path[-1])"`
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

notify_blacklist="bpython ygdb mc livestreamer okular zathura tmux less nano yvim mutt gmail cppman qvim fbless htop ranger mosh"

function store-command-stats() {
  last_command=$1
  last_command_name=${1[(wr)^(*=*|sudo|-*)]}
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
  if ! echo $notify_blacklist |  grep `echo $last_command_name | cut -d' ' -f1` >/dev/null 2>&1; then
    if [[ $last_status -gt "0" ]]; then
      notify-error "$start_time" "$last_command" 2>/dev/null
    elif [[ -n $start_time ]]; then
      notify-success "$start_time" "$last_command" 2>/dev/null
    fi
  fi
  unset last_command start_time last_status
}

alias x=xdg-open
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

function set-title() {
    echo -ne "\033]0;$@\007"
}

function update-term-title() {
    set-title `print -P %m: %~`
}
add-zsh-hook precmd update-term-title

function totp {
    eval `gpg -d ~/.secrets.gpg`
    eval oathtool --totp \$$1 --base32
}

if which aws >/dev/null; then
    source aws_zsh_completer.sh
fi
if which rg >/dev/null; then
    alias ag="rg";
fi
alias gst="git status"
alias lein-repl="LEIN_FAST_TRAMPOLINE=y lein trampoline run -m clojure.main"
alias popd="popd -q"
if which exa >/dev/null; then
    alias ls="exa"
    # https://github.com/ogham/exa
else
    alias ls='ls --classify --color --human-readable --group-directories-first'
fi
alias grep="grep --color -i -n "

alias less="less -r"

# Allow SSH tab completion for mosh hostnames
compdef mosh=ssh

alias json="python -m json.tool"
