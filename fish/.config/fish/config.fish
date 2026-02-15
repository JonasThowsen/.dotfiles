set -g fish_key_bindings fish_vi_key_bindings

alias lg lazygit
alias vim nvim
zoxide init fish | source

fish_add_path ~/.local/bin
fish_add_path ~/.opencode/bin
fish_add_path ~/.cargo/bin
