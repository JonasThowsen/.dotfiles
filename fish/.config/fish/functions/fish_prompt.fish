function fish_prompt
        # Emacs TRAMP logs in with TERM=dumb and waits for a prompt ending
        # in "$ "; it does not recognise the " ) " below
        if test "$TERM" = dumb
                echo -n '$ '
                return
        end
        set_color $fish_color_cwd
        echo -n (path basename $PWD)
        set_color normal
        echo -n ' ) '
end
