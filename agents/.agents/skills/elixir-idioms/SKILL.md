---
name: elixir-idioms
description: Work effectively and idiomatically in Elixir projects. Use this skill whenever the user asks for Elixir code, refactors, tests, debugging, or architectural changes in an Elixir codebase. Prefer current official Elixir and HexDocs references over memory, then align the implementation with existing local project patterns.
---

# Elixir Idioms

Use this skill when working in an Elixir codebase.

The goal is to avoid stale remembered patterns and instead use current official docs plus the project's existing conventions.

## Core workflow

Before making non-trivial Elixir changes:

1. Read `references/docs.md`.
2. Identify the exact Elixir feature, standard library module, or package involved.
3. Prefer current official docs and examples over memory.
4. Inspect the local codebase for existing patterns before introducing a new style.
5. Implement in a way that matches both official idioms and the project's conventions.

## What to prioritize

Prefer:
- official Elixir documentation for language and standard library behavior
- package docs on HexDocs for library APIs and examples
- existing local conventions for naming, module organization, return shapes, and tests

## Heuristics

- Keep modules focused.
- Prefer simple functions and straightforward data flow.
- Use pattern matching, function heads, and pipelines where they improve clarity.
- Do not invent abstractions when the local project already has a clear pattern.
- If unsure about API details or current best practice, check the docs rather than guessing.

## Deliverables

When relevant, briefly mention:
- which docs or package docs guided the implementation
- which local project pattern you matched

Keep the explanation short and practical.
