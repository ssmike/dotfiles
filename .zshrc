source ~/.zprofile 2>/dev/null

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
bindkey '^E' autosuggest-accept

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

typeset -A ZSH_HIGHLIGHT_STYLES

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

TIMEFMT='%U user %S system %P cpu %*E total'$'\n'\
'max resident memory:       %M MB'$'\n'\
'involountary switches:     %c'$'\n'\
'volountary switches:       %w'$'\n'\
'major page faults:         %F'$'\n'\
'minor page faults:         %R'

READNULLCMD=less
NOTIFY_COMMAND_TIMEOUT=90

alias ls='ls --color=auto'

export EDITOR=vim

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

function cvs_prompt() {
    if git branch >/dev/null 2>/dev/null; then
        ref=$(git symbolic-ref HEAD | sed -e "s/refs\/heads\///")
        echo -n "%F{red}git%f on %F{green}$ref%f"
    else
        echo -n ""
    fi
}

function root_shell() {
    export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
    ~/bin/zsh $@
}

# xterm directory
chpwd() {
    [[ -t 1 ]] || return
    case $TERM in
      sun-cmd) print -Pn "\e]l%~\e\\"
        ;;
      *xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%~\a"
        ;;
    esac
}

[ ! "$UID" = "0" ] && PROMPT='($(cvs_prompt))%B%F{blue}%2~%f%F{blue}%f%b> '
[  "$UID" = "0" ] && PROMPT='($(cvs_prompt))%B%F{red}%2~%f%F{blue}%f%b> '
RPROMPT="%{$fg_bold[grey]%}(%*)%{$reset_color%}%"

get_visible_length() {
    echo `echo $1 | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" | wc -m`
}

pre-prompt() {
  local exit_code=$?
  local PREPROMPT="%F{yellow}%m%f%F{blue}/%f"
  local PWD_STYLE="%B%F{blue}%2~%b%f"
  [  "$UID" = "0" ] && PWD_STYLE="%B%F{red}%2~%b%f"
  ZSH_CVS=`cvs_prompt`
  if [ ! -z "$ZSH_CVS" ]; then
    ZSH_CVS="$ZSH_CVS : "
  fi
  local LEFT="%B.%b%B%F{green}(%b$PREPROMPT$ZSH_CVS$PWD_STYLE%B%F{green})%b"
  if [ ! -z $VIRTUAL_ENV ]; then
    LEFT="$LEFT%F{red}[`echo $VIRTUAL_ENV | rev | cut -d'/' -f1 | rev`]%f"
  fi
  if [[ $exit_code != 0 ]]; then
    LEFT="$LEFT%f-%F{red}$exit_code%f"
  fi
  # -- color
  LEFT="$LEFT%B%f"
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
    LEFT="$LEFT%B"
    LEFT_P="$(print -P "$LEFT")"
    LEFTWIDTH=`get_visible_length "$LEFT_P"`
    RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH+$RIGHT_DELTA))
  fi
  LEFT_P="$(print -P "$LEFT")"
  RIGHT_P="$(print -P "$RIGHT")"
  LEFTWIDTH=`get_visible_length "$LEFT_P"`
  RIGHT_DELTA=$(($#RIGHT_P-`get_visible_length $RIGHT_P`))
  RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH))
  if [ $RIGHTWIDTH -lt 1 ]; then
    RPROMPT=""
    PROMPT='%B-%f%F{white}>%b%f '
  else
    PROMPT="$LEFT${(l:$RIGHTWIDTH::-:)RIGHT}"'
%B\`--%f%F{white}>%b%f '
    RPROMPT="%F{grey}%B(%*)%b%f"
  fi
}


add-zsh-hook precmd pre-prompt

chprompt() {
  RPROMPT=""
  PROMPT="%B%F{blue}%2~%f%F{blue}%f%b> "
}

function set-title() {
    echo -ne "\033]0;$@\007"
}

function update-term-title() {
    set-title `print -P %m: %~`
}
add-zsh-hook precmd update-term-title

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

zstyle ':completion:*:processes' menu yes select
zstyle ':completion:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -xuf'
zstyle ':completion:*:processes' sort false
zstyle ':completion:*:processes-names' command 'ps xho command'

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

# -[ history ]-
HISTFILE=~/.zsh_history
HISTSIZE=2000
SAVEHIST=1000

setopt append_history hist_ignore_all_dups hist_ignore_space autocd extendedglob autopushd
export LS_COLORS='*.py=3'

# -[ functions ]-

swap() {
    swap-impl() {
        if [ -z "$1" ]; then
            echo "wrong usage";
            exit 1;
        fi
        local binary_path=$1
        local binary_name=`echo $1 | rev | cut -d'/' -f1 | rev`
        target=`readlink /Berkanavt/bin/$binary_name`
        echo -n "cp $binary_path $target.new;"
        echo -n "mv $target.new $target;"
        typeset -A services
        services=(agglomerative_clusterd clusterd driver make_3days_news news_storage storage)
        if [[ $services[$binary_name] != "" ]]; then
            binary_name=$services[$binary_name]
        fi
        rc_script="/Berkanavt/bin/news/rc/$binary_name"
        rc_log="/Berkanavt/news/logs/$binary_name"
        if [ -z "$2" ]; then
            if ! [ -e "$rc_script" ]; then
                rc_script="$rc_script.russian";
                rc_log="$rc_log.russian";
            fi
        else
            rc_script="$rc_script.$2";
            rc_log="$rc_log.$2"
        fi
        echo -n "$rc_script restart;";
        echo -n "tail -f $rc_log.rc.log;";
    }
    echo `swap-impl $@`
    read "ret"
    sudo -u ynews bash -c "`swap-impl $@`"
}

get-arcadia() {
    local DIR=$1
    "svn" cat svn+ssh://arcadia.yandex.ru/arc/trunk/arcadia/ya | python - clone $DIR
    echo 'PATH=$PATH:'$DIR >> ~/.zprofile
    $DIR/ya completion --zsh
    chmod 755 -R ~/.ya.completion
    exec zsh
}

svn-cleanup() {
    ya tool svn status $1 | grep '^?' | awk '{print $2}' | xargs rm
}

name() {
    name=$1
    vared -c -p 'rename to: ' name
    command mv $1 $name
}

edit-cmd() {
    ffile="/tmp/.zsh-temp$RANDOM"
    touch $ffile
    vim $ffile -c "set filetype=zsh"
    . $ffile
}

#zle -N edit-cmd
bindkey -s "" edit-cmd

mkd() { mkdir $1; cd $1 }
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

enum() {
  cat $1 | sed = | sed -e 's/.*/    &/;s/.*\(.\{4\}\)$/\1/;N;s/\n/ /g'
}

notify_blacklist='ygdb mc tmux less nano yvim man htop ssh mosh tail watch cppman sky-tail screen'

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
    ya notify "$2 failed on `hostname`"
  fi
}

function notify-success() {
  local now diff start_time last_command

  start_time=$1
  last_command="$2"
  now=$(date "+%s")
  (( diff = $now - $start_time ))
  if (( $diff > $NOTIFY_COMMAND_TIMEOUT )); then
     ya notify "$2 finished on `hostname`"
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

# URL encode something and print it.
function url-encode; {
        setopt extendedglob
        echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

function arcanum() {
    curl -k https://rb.yandex-team.ru/arc/r/$1/diff/raw/
}

function pprof() {
    "$(ya tool gpt_perf --print-path)/gpt/bin/pprof" "$@"
}

add-zsh-hook preexec store-command-stats
add-zsh-hook precmd notify-command-complete

# -[ alias ]-
#alias -s avi=vlc --fbdev=/dev/fb0
alias -s jar=java -jar
alias -s fb2=fbless
alias -s cpp="vim"
alias -s h="vim"
alias -s pdf=okular
alias -s djvu=okular
alias -s hs=runhaskell
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

alias -g yvim="ya nvim"
alias popd="popd -q"
alias ls='ls --classify --color --human-readable --group-directories-first'
alias gateway='ip route | grep default | cut -d" " -f3'
alias grep="grep --color -i -n "
alias -g ygdb="ya tool gdb"
alias ymake="ya make -j20"
alias yag="ya tool ag"
alias -g jq="ya tool jq"
alias -g yvalgrind="ya tool valgrind"
alias json="python -m json.tool"
alias svn="ya tool svn"
alias hg="ya tool hg"
alias -g bn="/Berkanavt/news/"
alias sky-tail="sky run --stream"

if which rg >/dev/null; then
    alias ag="rg";
fi
if which exa >/dev/null; then
    alias ls="exa"
    # https://github.com/ogham/exa
else
    alias ls='ls --classify --color --human-readable --group-directories-first'
fi

if [ -d ~/.ya ]; then 
    export ASAN_SYMBOLIZER_PATH=`find ~/.ya/tools -name "*symbolizer*" | head -1`
    export MSAN_SYMBOLIZER_PATH=`find ~/.ya/tools -name "*symbolizer*" | head -1`
    source ~/.ya.completion/zsh/ya # YA_COMPLETION NAME='ya'
fi

typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[command]='bold'
