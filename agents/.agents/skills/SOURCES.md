# Skill sources

## dmmulroy/.dotfiles

The following skills were imported from [`dmmulroy/.dotfiles`](https://github.com/dmmulroy/.dotfiles/tree/main/home/.agents/skills) at commit `a7beb729d3a29237a3a02cb84d53e70fa9ab76a1` and lightly adapted for this cross-agent, Linux-based setup:

- `bro`
- `diagnosing-bugs`
- `domain-modeling`
- `grilling`
- `grill-me`
- `grill-with-docs`
- `handoff`
- `herdr`
- `prototype`
- `show-me`
- `tdd`
- `write-discoverable-code`
- `writing-for-agents`

The adaptations remove references to unavailable companion skills, make delegation optional, use `xdg-open`, and avoid assuming every project has an issue tracker.

Several of these skills originated in or were adapted from [Matt Pocock's skills repository](https://github.com/mattpocock/skills), which is MIT licensed. See [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md).

## Functional deslop

`deslop` is a local functional-programming and Elixir-focused synthesis. Before writing it, these public implementations were reviewed for general code-deslop patterns:

- [`pedronauck/skills` — `deslop`](https://github.com/pedronauck/skills/tree/main/skills/mine/deslop)
- [`boudra/unslop`](https://github.com/boudra/unslop)
- [`LeonardNJU/code-humanizer`](https://github.com/LeonardNJU/code-humanizer)
- [`poweroftrue/slop-skills`](https://github.com/poweroftrue/slop-skills)

The public `mattpocock/skills` repository at `5b15a47f2d7150f545fbcacbfe381787fc0230dc` and `kitlangton/skills` at `0cace2ae0bd65e0cb03ab12860b62ae5e043f0df` were also checked; neither contained a code deslop skill.
