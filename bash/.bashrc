# Makes terminal resize properly
shopt -s checkwinsize

PROMPT_COMMAND='
  # Colors
  RESET="\[\033[0m\]"
  DIM="\[\033[2m\]"
  CYAN="\[\033[36m\]"
  YELLOW="\[\033[33m\]"
  RED="\[\033[31m\]"
  WHITE="\[\033[37m\]"

  # Git branch info
  git_branch=$(git branch --show-current 2>/dev/null)
  dirty=""

  if [ -n "$git_branch" ]; then
    dirty=$(git status --porcelain 2>/dev/null)
    [[ -n "$dirty" ]] && dirty="${RED}*${RESET}" || dirty=""
  fi

  if [[ -n "$IN_NIX_SHELL" && -n "$git_branch" ]]; then
    PS1="${DIM}\W${RESET}(nix)${CYAN}[${git_branch}${RESET}${dirty}${CYAN}]${RESET}${WHITE}> ${RESET}"
  elif [ -n "$git_branch" ]; then
    PS1="${DIM}\W${RESET}${CYAN}[${git_branch}${RESET}${dirty}${CYAN}]${RESET}${WHITE}> ${RESET}"
  elif [ -n "$IN_NIX_SHELL" ]; then
    PS1="${DIM}\W${RESET}(nix)${WHITE}> ${RESET}"
  else
    PS1="${DIM}\W${RESET}${WHITE}> ${RESET}"
  fi
'

alias lg="lazygit"
alias vim="nvim"

# Quick session switcher with fzf
ts() {
    # List all sessions, or create new one
    local session
    session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --print-query | tail -1)
    if [[ -n "$session" ]]; then
        if tmux has-session -t="$session" 2>/dev/null; then
            # If we're in tmux, switch to session, otherwise attach
            if [[ -n "$TMUX" ]]; then
                tmux switch-client -t "$session"
            else
                tmux attach -t "$session"
            fi
        else
            tmux new-session -d -s "$session" && tmux switch-client -t "$session"
        fi
    fi
}

export PATH="$HOME/.local/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"

if [[ $- == *i* ]]; then
  bind '"\C-f": "tmux-sessionizer\n"'
fi

export TERM=tmux-256color

eval "$(zoxide init bash)"
eval "$(direnv hook bash)"
