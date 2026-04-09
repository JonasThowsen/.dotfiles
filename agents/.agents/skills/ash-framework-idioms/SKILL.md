---
name: ash-framework-idioms
description: Build and modify Ash Framework code using current Ash-native patterns. Use this skill whenever the user asks for Ash resources, actions, relationships, calculations, changes, validations, policies, code interfaces, or Ash refactors/debugging. Prefer current official Ash documentation and examples over memory, then align with local project conventions.
---

# Ash Framework Idioms

Use this skill when working with Ash Framework code.

The goal is to use Ash the way Ash expects to be used, instead of recreating custom service layers or stale patterns from memory.

## Core workflow

Before making non-trivial Ash changes:

1. Read `references/docs.md`.
2. Identify the exact Ash feature involved.
3. Prefer current official Ash docs and examples over memory.
4. Inspect the local codebase for existing resources, actions, policies, and interfaces.
5. Implement using Ash-native patterns unless the project clearly has a justified alternative.

## What to prioritize

Prefer:
- current official Ash documentation
- current package docs for related Ash packages
- existing local patterns for resources, actions, relationships, policies, and domain organization

## Heuristics

- Model behavior in Ash-native constructs when appropriate.
- Avoid hand-rolled patterns when Ash already provides a clear mechanism.
- Check docs before implementing actions, changes, validations, calculations, aggregates, relationships, or policies.
- Match the existing repo's Ash style before inventing new conventions.
- Do not trust memory for Ash APIs when the docs are easily available and usually better.

## Deliverables

When relevant, briefly mention:
- which Ash docs or examples informed the implementation
- which local project pattern you matched

Keep the explanation short and practical.
