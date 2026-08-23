---
name: deslop
description: Remove AI-generated code slop from functional code and diffs without changing behavior. Use when the user asks to deslop, unslop, simplify, clean up AI-written code, remove boilerplate or over-engineering, or make code more idiomatic. Applies functional-programming principles across languages, with additional Elixir guidance.
---

# Functional Deslop

Remove code that works but leaves the codebase noisier, less idiomatic, or harder to change. Favor direct data transformations, explicit effects, stable domain shapes, and the smallest abstraction that serves a current need.

This is a behavior-preserving cleanup workflow, not a bug hunt or a license for broad refactoring.

## 1. Establish scope and intent

Honor an explicit path, diff, branch, commit range, or PR. Otherwise:

- after work in the current session, inspect that work's staged, unstaged, and relevant untracked changes;
- on a feature branch, compare against its merge base with the default branch or upstream;
- if neither is clear, ask rather than guessing a large scope.

Read the request or spec behind the change. Check `git status --short`, preserve unrelated user changes, and read every affected file in full. Judge new code against the surrounding module, not against a generic style guide.

Use report-only mode when the user asks for an audit or review. An explicit request to clean up or deslop authorizes focused edits within the agreed scope.

## 2. Load the applicable idioms

Read [references/functional-programming.md](references/functional-programming.md) for every task.

For Elixir code, also:

1. Read [references/elixir.md](references/elixir.md).
2. Read `../elixir-idioms/SKILL.md` and its documentation references.
3. If the affected code uses Ash or LiveView, read the corresponding local idiom skill before proposing framework-level simplifications.

Repository conventions and current language or framework documentation override a generic preference in this skill.

## 3. Build a behavior-preservation check

Run the narrowest relevant existing tests or checks before editing when practical. Record pre-existing failures. If the target has no meaningful verification, default to reporting risky cleanup rather than silently changing error shapes, ordering, timing, or side effects.

Formatting and static analysis are useful checks, but they do not prove behavior preservation.

## 4. Audit for slop

Search the repository before calling something duplicated or unnecessary. A finding needs concrete evidence and a smaller target shape.

Review for these categories:

- **Noise:** comments that narrate syntax, boilerplate docs, debug residue, stale compatibility code, or helpers with no caller.
- **Reinvention:** local copies of an existing project helper, standard combinator, library feature, or framework capability.
- **Speculative structure:** one-use interfaces, behaviours, callbacks, registries, wrappers, configurable machinery, or public options added for hypothetical reuse.
- **Imperative translation:** mutation-shaped control flow mechanically transcribed into recursion, folds, state holders, processes, or effectful callbacks when a direct transformation exists.
- **Effect sprawl:** I/O, logging, configuration lookup, time, randomness, or process state mixed through otherwise pure transformations instead of being owned at a boundary.
- **Uncommitted shapes:** catch-all branches, nil/default tunnels, string-or-atom keys, map-or-struct handling, boolean flag combinations, or repeated validation of values already normalized upstream.
- **Pipeline and combinator theater:** chains split into tiny one-use helpers, opaque point-free code, gratuitous folds, or pipelines that hide branching and effects rather than clarify data flow.
- **Error flattening:** broad rescue/catch, swallowed errors, lossy conversion of distinct failures, or defensive fallbacks unsupported by the actual contract.
- **Concurrency theater:** actors, processes, tasks, queues, or parallel traversal added without a state-ownership, isolation, latency, or measured throughput need.
- **Test slop:** tests coupled to private helpers, mock choreography, tautological assertions, duplicated cases, or tests added only to bless the generated implementation shape.

Do not flag code merely because it is verbose, impure, recursive, object-oriented, or unfamiliar. Boundaries have effects. State can be real. Explicit code can be clearer than clever composition. Require a concrete maintenance cost and evidence that the proposed form fits this repository.

## 5. Rank and fix

For each candidate, identify:

- file and line;
- the concrete cost;
- evidence from callers, local conventions, or an existing capability;
- the smallest behavior-preserving replacement;
- any risk to errors, ordering, laziness, concurrency, or side effects.

Drop style-only, speculative, already-solved, or behavior-changing candidates. In report-only mode, return the surviving findings and stop.

When editing:

1. Prefer deletion, direct transformation, or reuse over introducing another abstraction.
2. Keep each change local to the reviewed behavior.
3. Preserve public return values, error variants, effect order, message order, laziness/eagerness, and concurrency semantics unless the user separately approves a behavior change.
4. Do not clean unrelated pre-existing code opportunistically.
5. Re-read the final diff for over-correction; idiomatic functional code is not code golf.

## 6. Verify and report

Run the relevant formatter, focused tests, static checks, and `git diff --check`. Re-run the original behavior check after editing.

Report:

- what was removed or simplified and why;
- meaningful candidates skipped because they were justified or behavior-changing;
- verification run and any pre-existing or unresolved failures.

If no candidate survives verification, say the code is already clean under this skill's rules. Do not manufacture findings.
