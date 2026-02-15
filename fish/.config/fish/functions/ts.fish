function ts --description "Quick tmux session switcher with fzf"
    set -l session (tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --print-query | tail -1)
    if test -n "$session"
        if tmux has-session -t="$session" 2>/dev/null
            if test -n "$TMUX"
                tmux switch-client -t "$session"
            else
                tmux attach -t "$session"
            end
        else
            tmux new-session -d -s "$session" && tmux switch-client -t "$session"
        end
    end
end
