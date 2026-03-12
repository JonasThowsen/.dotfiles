function mtr
    set file (fd . test -e exs | fzf \
        --prompt="Test file > " \
        --height=80% \
        --reverse \
        --preview 'bat --style=numbers --color=always {}')

    if test -z "$file"
        return
    end

    set match (rg --line-number '^\s*test\s+"' $file | fzf \
        --prompt="Test in "(basename $file)" > " \
        --height=80% \
        --reverse \
        --delimiter ':' \
        --preview "bat --style=numbers --color=always $file --highlight-line {1}")

    if test -z "$match"
        mix test "$file"
        return
    end

    set line (string split ":" $match)[1]
    mix test "$file":"$line"
end
