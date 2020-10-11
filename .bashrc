# .bashrc of Patrick Winter <patrickwinter@posteo.ch>

# Don't load .bashrc further if I connect with TRAMP to this machine.
if [[ $TERM = dumb ]]; then
    return
fi

# Use emacs keybindings
set -o emacs

# Environment variables
export EDITOR="emacsclient -nw"

# Password store
export PASSWORD_STORE_DIR="$HOME/vcs/passwords/"

export PATH="$HOME/bin:$PATH"

# The emacs solarized theme requires true color
export TERM="xterm-24bit"

# My pseudo true color setup breaks in SSH sessions
alias ssh="TERM=xterm-256color ssh"

# Ignore commands that start with a whitespace.
export HISTCONTROL='ignorespace:erasedups'

# Store timestamps of commands .bash_history
export HISTTIMEFORMAT='%Y-%m-%d %T '

# Ignore with less then 3 characters
export HISTIGNORE='?:??:history'

# Append to .bash_history instead of overwritting it
shopt -s histappend

# Don't autoexecute history expansion (e.g. !!), show them for review.
shopt -s histverify

# Combine multiline commands into one in history
shopt -s cmdhist

# Unlimited number of lines in .bash_history
export HISTFILESIZE=-1

# Unlimited number of lines that are stored in memory while running a bash session
export HISTSIZE=10000

# Solarized colors
BASE03=$(tput setaf 234)
BASE02=$(tput setaf 235)
BASE01=$(tput setaf 240)
BASE00=$(tput setaf 241)
BASE0=$(tput setaf 244)
BASE1=$(tput setaf 245)
BASE2=$(tput setaf 254)
BASE3=$(tput setaf 230)
YELLOW=$(tput setaf 136)
ORANGE=$(tput setaf 166)
RED=$(tput setaf 160)
MAGENTA=$(tput setaf 125)
VIOLET=$(tput setaf 61)
BLUE=$(tput setaf 33)
CYAN=$(tput setaf 37)
GREEN=$(tput setaf 64)
BOLD=$(tput bold)
RESET=$(tput sgr0)

PS1="\[${RESET}\]\[${BOLD}\]\w\[${RESET}\]\[${GREEN}\]\$(__git_ps1) \[${RESET}\]"

export PROMPT_COMMAND="history -a; history -n"

# Set terminal title initially to $PWD
echo -en "\033]0;$(hostname):$(pwd) \a"

# Base
alias l='ls -CF'
alias ls="ls --color"
alias la='ls -ACF'
alias ll='ls -AhlF'
alias h='history'
alias grep='grep --color=auto'

# Aliases
alias e='emacsclient -nw'
alias da='direnv allow'
alias mg="emacsclient -nw -e '(progn (magit-status) (delete-other-windows))'"
alias ediff='emacsclient -c -a emacs -q --eval "(ediff-files \"$1\" \"$2\")";'
alias er='systemctl --user restart emacs'
alias g='git'
alias t='tmux'
alias m='make'
alias dc="docker-compose"
alias o='xdg-open'
alias b="firefox"

# Python virtualenv
alias ae='deactivate &> /dev/null; source ./env/bin/activate'
alias de='deactivate'

# Navigation
alias ..="cd .."
alias ...="cd ../.."
alias cdv='cd ~/vcs'
alias cdm='cd /mnt'
alias cdf='cd ~/shared/fhnw'
alias cds='cd ~/shared'

# Store global npm packages in home directory
export NPM_CONFIG_PREFIX="$HOME/.npm/node_modules/"
export PATH="$PATH:$HOME/.npm/node_modules/bin"

# Set GOPATH to hidden directory in home directory
export GOPATH="$HOME/.go/"
export PATH="$PATH:$HOME/.go/bin"

# dircolors
test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)

# fzf solarized dark theme
export FZF_DEFAULT_OPTS='
  --color=bg+:#073642,bg:#002b36,spinner:#719e07,hl:#586e75
  --color=fg:#839496,header:#586e75,info:#cb4b16,pointer:#719e07
  --color=marker:#719e07,fg+:#839496,prompt:#719e07,hl+:#719e07'
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# direnv
eval "$(direnv hook bash)"

# fasd
alias j='fasd_cd -d'
eval "$(fasd --init auto)"

