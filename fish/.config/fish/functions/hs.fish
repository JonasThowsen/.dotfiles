function hs --description "Quick Herdr session switcher with fzf"
    if not command -q herdr
        echo "hs: herdr is not installed" >&2
        return 1
    end

    if not command -q fzf
        echo "hs: fzf is not installed" >&2
        return 1
    end

    if not command -q fd
        echo "hs: fd is not installed" >&2
        return 1
    end

    if not command -q jq
        echo "hs: jq is not installed" >&2
        return 1
    end

    set -l roots $argv
    if test (count $roots) -eq 0
        if set -q HERDR_SESSION_DIRS[1]
            set roots $HERDR_SESSION_DIRS
        else
            set roots ~/coding ~/dotfiles ~
        end
    end

    set -l dir (
        for root in $roots
            if test -d "$root"
                realpath "$root"
                fd --type directory --max-depth 2 --hidden --exclude .git . "$root"
            end
        end | sort -u | fzf --prompt="herdr session> "
    )

    if test -z "$dir"
        return 0
    end

    set -l session (basename "$dir" | string replace -a -r '[^A-Za-z0-9_.-]' '_')
    if test -z "$session"
        echo "hs: could not derive a session name from $dir" >&2
        return 1
    end

    if herdr session list --json 2>/dev/null | jq -e --arg name "$session" '.sessions[] | select(.name == $name)' >/dev/null
        herdr session attach "$session"
    else
        pushd "$dir" >/dev/null
        herdr --session "$session"
        popd >/dev/null
    end
end
