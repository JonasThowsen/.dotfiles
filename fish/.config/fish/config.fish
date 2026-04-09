alias ssh "kitty +kitten ssh"
alias lg lazygit
alias vim nvim
alias nd "nix develop --command fish"
zoxide init fish | source

set -gx PATH $PATH $HOME/.local/bin $HOME/.opencode/bin $HOME/.cargo/bin $HOME/.npm-global/bin
