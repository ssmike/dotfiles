source /etc/profile
source ~/.profile
source virtualenvwrapper.sh
#export WINEARCH=win32


export GOOGLE=8.8.8.8
export PATH=$PATH:~/.cabal/bin
export BROWSER=chromium-browser

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
typeset -A ZSH_HIGHLIGHT_STYLES

ZSH_HIGHLIGHT_STYLES=(
        'alias'           'fg=green'
        'builtin'         'fg=cyan'
        'function'        'fg=cyan'
        'command'         'fg=255,bold'
        'precommand'      'fg=magenta, underline'
        'hashed-commands' 'fg=cyan'
        'path'            'underline'
        'globbing'        'fg=166'
)

#export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on"
READNULLCMD=less

#insert-square-brackets() {
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

NOTIFY_ICON="/usr/share/icons/gnome/32x32/apps/konsole.png"
NOTIFY_COMMAND_TIMEOUT=30
export _JAVA_AWT_WM_NONREPARENTING=1


#CLAWS='claws-mail --status | sed -e "s/^[0-9]*\ \([0-9]*\).*/\1/g" | sed -e "s/^$/0/g"'
#[ ! "$UID" = "0" ] && [ $(bash -c $CLAWS) != 0 ] && echo "===========You have unread mail===========";

if [[ $TERM == linux ]]; then
    setfont cyr-sun16
else
  export TERM=xterm-256color
fi;

alias ls='ls --color=auto'


export EDITOR=vim

bindkey "^[[0~" beginning-of-line
bindkey "^[OH" beginning-of-line
bindkey "^[[H" beginning-of-line
bindkey "[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[F" end-of-line
bindkey "^[OF" end-of-line
bindkey "[4~" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[[3~" delete-char
bindkey "^[[3~" delete-char
bindkey "[3~" delete-char
bindkey "^[[1;5C" forward-word
bindkey "OC" forward-word
bindkey "^[[1;5D" backward-word
bindkey "OD" backward-word
bindkey "[5~" up-line-or-history
bindkey "[6~" down-line-or-history
bindkey "[2~" quote-line
bindkey "^R" history-incremental-search-backward
bindkey "^[[1;3D" backward-delete-word
bindkey "^[[1;3C" delete-word


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
  local PWD_STYLE="%B%F{blue}%2~%b%f"
  [  "$UID" = "0" ] && PWD_STYLE="%B%F{red}%2~%b%f"
  local LEFT="%F{black}%B.%b%f%B%F{green}(%b$(cvs_prompt) : $PWD_STYLE%B%F{green})%b"
  if [ ! -z $VIRTUAL_ENV ]; then
    LEFT="$LEFT%F{red}[`echo $VIRTUAL_ENV | cut -d'/' -f5`]%f"
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
    LEFT="%F{black}%B.%b%f%B%F{green}(%b$PWD_STYLE%B%F{green})%b"
    LEFT="$LEFT%F{black}%B"
    LEFT_P="$(print -P "$LEFT")"
    LEFTWIDTH=`get_visible_length "$LEFT_P"`
    RIGHTWIDTH=$(($COLUMNS-$LEFTWIDTH+$RIGHT_DELTA))
  fi
  print  $LEFT_P${(l:$RIGHTWIDTH::-:)RIGHT_P}
  PROMPT='%F{black}%B\`--%f%F{white}>%b%f '
  RPROMPT="%F{grey}%B(%*)%b%f"
}


add-zsh-hook precmd pre-prompt

chprompt() {
  RPROMPT=""
  PROMPT="%B%F{blue}%2~%f%F{blue}%f%b> "
}

# -[ completion ]-
autoload -Uz compinit
compinit
source aws_zsh_completer.sh

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

#source ~/.zsh/zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically.
#zle-line-init() {
#    zle autosuggest-start
#}
#zle -N zle-line-init

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
name() {
    name=$1
    vared -c -p 'rename to: ' name
    command mv $1 $name
}
alias composite="compton -cCGf"
alias yandex="cadaver https://webdav.yandex.ru"

edit-cmd() {
    ffile="/tmp/.zsh-temp$RANDOM"
    touch $ffile
    vim $ffile -c "set filetype=zsh"
    . $ffile
}

#zle -N edit-cmd
bindkey -s "" edit-cmd

mkd() { mkdir $1; cd $1 }
battcheck() {
  (acpi -b | python -c "if int(input().split()[3].split('%')[0]) < 20: exit(1)");
}

makeproject() {
    if [ "$#" -ne 2 ]; then
        echo "Wrong usage"
        return 1
    fi
    mkdir $2
    cd $2
    cp ~/templates/$1/* . -r
    make edit
}

alias mkproject="makeproject c++ "
alias mkjproject="makeproject java "
alias mkpproject="makeproject python "

alias rmrf="rm -rf $1"

log() {
    killall conky -SIGSTOP
    (bash -c $1) > ~/.info
    sleep 2s
    killall conky -SIGCONT
}
rep() {
  while true; do
    zsh -c $1;
    sleep 4;
  done;
}
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

blacklist_regexp="^\(bpython|gdb|mc|livestreamer|okular|tmux|less|nano|vim|mutt|man|qvim|gdb|fbless|htop\).*"

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
    notify-send -i $NOTIFY_ICON "$2 failed";
  fi
}

function notify-success() {
  local now diff start_time last_command

  start_time=$1
  last_command="$2"
  now=$(date "+%s")
  (( diff = $now - $start_time ))
  if (( $diff > $NOTIFY_COMMAND_TIMEOUT )); then
    notify-send -i $NOTIFY_ICON "$2 finished";
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
alias -s jar=java -jar
alias -s fb2=fbless
alias -s cpp=vim
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

# URL encode something and print it.
function url-encode; {
        setopt extendedglob
        echo "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

# Search google for the given keywords.
function google; {
        $BROWSER "http://www.google.com/search?q=`url-encode "${(j: :)@}"`"
}
function slovari {
        $BROWSER "https://translate.yandex.ru/?text=`url-encode "${(j: :)@}"`&lang=en-ru"
}
alias t=slovari

function keyw {
  sudo $EDITOR /etc/portage/package.accept_keywords/$1
}

function use {
  sudo $EDITOR /etc/portage/package.use/$1
}

function debug-flags; {
    echo -Wall -Wextra -pedantic -std=c++11 -O2 -Wshadow -Wformat=2 -Wfloat-equal -Wconversion -Wlogical-op -Wcast-qual -Wcast-align -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsanitize=address -fsanitize=undefined -fstack-protector -lmcheck -D_FORTIFY_SOURCE=2
}

function xsearch() {
  grep --color -i $1 ~/.local/share/applications/{mimeapps.list,mimeinfo.cache} /usr/share/applications/mimeinfo.cache
}

function xset() {
  xdg-mime default $2 $1
}

function xquery() {
  xdg-mime query default $1
}

#gentoo aliases
alias ascedit='vim ~/.local/share/applications/mimeapps.list ~/.local/share/applications/mimeinfo.cache /usr/share/applications/mimeinfo.cache'
#alias cp='cp --reflink=auto'
alias popd="popd -q"
alias femerge='sudo env FEATURES="-collision-detect -protect-owned" emerge'
alias getmail="/usr/share/scripts/mail.sh"
#alias emacsd="/bin/emacs --daemon"
#alias emacs="emacsclient -c -a=vim"
alias ls='ls --classify --color --human-readable --group-directories-first'
alias battery="acpi -b | sed -e 's/.* \([0-9]*\)%.*$/\1/g'"
alias printFile="gtklp"
alias akos-proxy="ssh -D 5222 akos -N"
alias pasteit='pastebinit -b "http://slexy.org"'
alias gateway='ip route | grep default | cut -d" " -f3'
alias grep="grep --color -i -n "
#alias compile="make 2>./compile-output"

prof() {
    gprof $1 | vim -
}
alias pulse="pulseaudio -k && pulseaudio --start"
alias vncreadonly="x11vnc -usepw -forever -viewonly"
