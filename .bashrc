# .bashrc of Patrick Winter <patrickwinter@posteo.ch>

# Don't load .bashrc further if I connect with TRAMP to this machine
if [[ $TERM = dumb ]]; then
    return
fi

# Use emacs keybindings
set -o emacs

# Environment variables
export EDITOR="emacsclient -nw"

# Add custom scripts to $PATH
export PATH="$HOME/bin:$PATH"

# Ignore commands that start with a whitespace and only deduplicate history
export HISTCONTROL='ignorespace:erasedups'

# Store timestamps of commands .bash_history
export HISTTIMEFORMAT='%Y-%m-%d %T '

# Ignore with less then 3 characters
export HISTIGNORE='?:??:history'

# Append to .bash_history instead of overwritting it
shopt -s histappend

# Don't autoexecute history expansion (e.g. !!), show them for review
shopt -s histverify

# Combine multiline commands into one in history
shopt -s cmdhist

# Use embedded newlines rather than semicolons when storing multiline commands
shopt -s lithist

# Unlimited number of lines in .bash_history
export HISTFILESIZE=-1

# Unlimited number of lines that are stored in memory while running a bash session
export HISTSIZE=100000

# Append unflushed in-memory history entries to ~/.bash_history and load missing
# history entries from file into memory.
export PROMPT_COMMAND="history -a; history -c; history -r"

# Typing a directory name just by itself will automatically change
# into that directory.
shopt -s autocd

# Automatically fix directory name typos when changing directory.
shopt -s cdspell

# Automatically expand directory globs and fix directory name typos whilst
# completing. Note, this works in conjuction with the cdspell option listed
# above.
shopt -s direxpand dirspell

# Enable the ** globstar recursive pattern in file and directory expansions
shopt -s globstar

# Prompt format.
BOLD=$(tput bold)
RESET=$(tput sgr0)
PS1="\[${RESET}\]\[${BOLD}\]\w\[${RESET}\] "

# Functions
o() {
  case "$1" in
    http://*|https://*)
      firefox "$1" &
      ;;
    *.pdf)
      zathura "$1" &
      ;;
    *.mkv|*.mp4)
      mpv "$1" &
      ;;
    *.jpg|*.jpeg|*.png|*.tiff)
      feh "$1" &
      ;;
    *)
      command /run/current-system/sw/bin/xdg-open "$@" &
      ;;
  esac
}

# Aliases
alias l='ls -CF'
alias ls="ls --color"
alias la='ls -ACF'
alias ll='ls -AhlF'
alias c="cd"
alias ..="cd .."
alias ...="cd ../.."
alias cdv='cd ~/vcs'
alias cds='cd ~/shared'
alias h='history'
alias grep='grep --color=auto'
alias e='emacsclient -nw'
alias r='just'
alias m='make'
alias da='direnv allow'
alias er='systemctl --user restart emacs'
alias g='git'
alias t='tmux'
alias p='pytest'
alias dc="docker-compose"
alias b="firefox"
alias w="watch "
alias zb="zig build"
alias zt="zig test"
alias zr="zig run"
alias k="kubectl"
alias ns="kubectl config view --minify --output 'jsonpath={..namespace}'; echo"
alias ctx="kubectl config view --minify --output 'jsonpath={..context.cluster}'; echo"
alias kctx="kubectx"
alias kns="kubens"
alias ae='deactivate &> /dev/null; source .venv/bin/activate'
alias de='deactivate'

# Configure completion for shell aliases
if command -v complete_alias &>/dev/null; then
    source "$(command -v complete_alias)"
    complete -F _complete_alias r k g dc t
fi

# Wire up fuzzy finder
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Wire up direnv
eval "$(direnv hook bash)"

# Wire up zoxide
eval "$(zoxide init --cmd j bash)"
