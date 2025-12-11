if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/jonas/.opam/opam-init/init.sh' && . '/home/jonas/.opam/opam-init/init.sh' > /dev/null 2> /dev/null || true
# END opam configuration
