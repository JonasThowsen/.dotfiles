---
name: nix-development
description: Work safely and correctly in Nix-managed development environments. Use this skill whenever the user is working in a repository that may use Nix, on NixOS, or when build/test/dev commands might depend on a flake dev shell. Always check for a flake.nix before running project commands, prefer the existing dev shell when already inside one, and use `nix develop . -c ...` when tools are not available outside the shell.
---

# Nix Development

Use this skill to avoid running project commands outside the environment they were designed for.

On this machine, Nix usage is common and repositories may expect commands to run inside a flake-based dev shell. Many failures that look like missing tools, wrong versions, or inconsistent formatting are really just "not inside the right Nix environment yet."

## Core behavior

Before running project-specific build, test, lint, format, generation, or dev-server commands:

1. Check whether the project uses Nix, starting with `flake.nix` in the current directory.
2. If needed, also look in reasonable parent directories when working inside a subdirectory of a repo.
3. If a relevant `flake.nix` exists, prefer running commands within that flake's dev shell.
4. If already inside a Nix dev shell, keep using it instead of wrapping every command again.

Do not make a big ceremony out of this. Just verify the environment early so the rest of the work is reliable.

## Practical workflow

### 1) Look for a flake first

At the start of work on a project, check for:

- `flake.nix`
- optionally `flake.lock` as supporting evidence

Usually the nearest relevant `flake.nix` in the repo is the right one.

If there is no `flake.nix`, continue normally unless the user tells you there is another Nix entrypoint.

### 2) Detect whether you are already in a dev shell

If the environment already indicates a Nix shell, prefer using it directly.

Useful signals include:

- `IN_NIX_SHELL`
- project tools already resolving correctly on `PATH`
- the user explicitly saying they are already in `nix develop`

When already inside the right shell, run commands normally.

### 3) If not in the shell, use the flake

When a project command depends on the development environment and you are not already in it, run it via:

```bash
nix develop . -c <command>
```

If the relevant `flake.nix` is in a parent or different project directory, run the command from that directory or use an equivalent form that targets the correct flake.

## When to use `nix develop . -c`

Use it for commands such as:

- tests
- builds
- formatters
- linters
- code generators
- local dev servers
- framework CLIs
- language toolchains provided by the flake

Examples:

```bash
nix develop . -c npm test
nix develop . -c cargo test
nix develop . -c mix test
nix develop . -c just check
```

## When not to overuse it

You usually do **not** need `nix develop` for simple file inspection or basic shell builtins.

Examples that normally do not need wrapping:

- reading files
- searching the tree
- listing directories
- small POSIX utilities that are already available

The goal is reliability, not ritual.

## Failure recovery

If a command fails with errors that suggest missing binaries, wrong tool versions, or an unavailable runtime:

1. Re-check whether a `flake.nix` exists.
2. Re-check whether you are already in the right shell.
3. Retry the command with `nix develop . -c ...` if you were not already using it.

Treat Nix environment mismatch as an early hypothesis, not a last resort.

## Communication with the user

Be brief and practical. You usually do not need to explain Nix unless it affects the work.

Good examples:

- "I found a `flake.nix`, so I’ll run tests through `nix develop` to match the project environment."
- "This repo has a flake and the tool isn’t on PATH outside the shell, so I’m retrying inside the dev shell."

## Summary

For project work on this machine:

- check for `flake.nix` early
- prefer the repo's flake-based dev shell
- if already in-shell, use it directly
- otherwise run project commands with `nix develop . -c ...`
- suspect environment mismatch when tools are missing
