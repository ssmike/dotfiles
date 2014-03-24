source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES=(
        'alias'           'fg=green'
        'builtin'         'fg=cyan'
        'function'        'fg=cyan'
        'command'         'fg=153,bold'
        'precommand'      'fg=magenta, underline'
        'hashed-commands' 'fg=cyan'
        'path'            'underline'
        'globbing'        'fg=166'
)

NOTIFY_ICON="/usr/share/icons/gnome/32x32/apps/konsole.png"
NOTIFY_COMMAND_TIMEOUT=30

#export TERM=xterm-256color
export _JAVA_AWT_WM_NONREPARENTING=1
[ ! "$UID" = "0" ] && [ ! -z "`find ~/mail/new -type f`" ] && echo "===========You have unread mail===========";

if [[ $TERM == linux ]]; then
    setfont cyr-sun16
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

setopt prompt_subst
autoload -U promptinit
promptinit
function chprompt(){
	prompt adam2;
}
bindkey -s "" 'chprompt'

#[ ! "$UID" = "0" ] && PROMPT='%B%F{blue}%n@%m%f%F{blue}%f%b%(!.#.$) '
[ ! "$UID" = "0" ] && PROMPT='%B%F{blue}%~%f%F{blue}%f%b> '
[  "$UID" = "0" ] && PROMPT='%B%F{red}%~%f%F{blue}%f%b> '
RPROMPT="%{$fg_bold[grey]%}(%*)%{$reset_color%}%"

# -[ completion ]-
autoload -Uz compinit
compinit
zstyle ':completion:*' menu yes select
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

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
    rm $ffile
}

#zle -N edit-cmd
bindkey -s "^X" "edit-cmd"

mkd() { mkdir $1; cd $1 }
battcheck() {
	(acpi -b | python -c "if int(input().split()[3].split('%')[0]) < 20: exit(1)");
}

makeproject() {
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
  if [[ $last_status -gt "0" ]]; then
    notify-error "$start_time" "$last_command" 2>/dev/null
  elif [[ -n $start_time ]]; then
    notify-success "$start_time" "$last_command" 2>/dev/null
  fi
  unset last_command start_time last_status
}

function op() {
    vblank_mode=0 exec primusrun $1
}

add-zsh-hook preexec store-command-stats
add-zsh-hook precmd notify-command-complete

# -[ alias ]-
#alias -s avi=vlc --fbdev=/dev/fb0
alias -s fb2=fbless
alias -s cpp=vim
alias -s pdf=zathura
alias -s djvu=zathura
#alias -s mkv=vlc --fbdev=/dev/fb0
#alias -s mp4=vlc --fbdev=/dev/fb0
#alias -s mov=vlc --fbdev=/dev/fb0
alias -s avi=smplayer
alias -s mkv=smplayer
alias -s mp4=smplayer
alias -s mov=smplayer
alias -s exe=wine
alias -s EXE=wine
alias -s vim="vim -S "
#gentoo aliases
alias popd="popd -q"
alias femerge='FEATURES="-collision-detect -protect-owned" emerge'
alias getmail="/usr/bin/getmail --rcfile=sms --rcfile=aesc --rcfile=mike"
alias emacsd="/bin/emacs --daemon"
alias emacs="emacsclient -c -a=vim"
alias ls='ls --classify --color --human-readable --group-directories-first'
alias homefree="df -h | grep home | sed -e 's/\([^ ]*[ ]*\)\{3\}\([^ ]*\)\([ ]*[^ ]*\)\{2\}/\2/g'"
alias battery="acpi -b | sed -e 's/.* \([0-9]*\)%.*$/\1/g'"
alias printFile="gtklp"
alias akos-proxy="ssh -D 5222 akos -N"
#alias compile="make 2>./compile-output"
prof() {
    gprof $1 | vim -
}
alias pulse="pulseaudio -k && pulseaudio --start"
#alias cp='nocorrect cp --interactive --verbose --recursive --preserve=all'
#alias mv='nocorrect mv --verbose --interactive'
#alias rm='nocorrect rm -Irv'
#alias mkdir='nocorrect mkdir'
