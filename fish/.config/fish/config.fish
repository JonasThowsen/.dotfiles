alias ssh="ghostty +ssh --"
alias lg lazygit
alias vim nvim
alias nd "nix develop --command fish"
zoxide init fish | source

# Persistent Herdr panes can outlive the graphical session that created them.
# Refresh stale X11 variables before launching commands so clipboard access keeps
# working after Xorg restarts on a different display number.
function __refresh_x11_environment --on-event fish_preexec
    if not set -q DISPLAY; or xset -display "$DISPLAY" q >/dev/null 2>&1
        return
    end

    for variable in DISPLAY XAUTHORITY
        set -l value (systemctl --user show-environment | string match "$variable=*" | string replace "$variable=" "")
        if test -n "$value"
            set -gx $variable "$value"
        end
    end
end

set -gx PATH $HOME/.local/bin $HOME/.opencode/bin $HOME/.cargo/bin $HOME/.npm-global/bin $HOME/.config/emacs/bin $HOME/.dotnet/tools $PATH
