---
name: liveview-idioms
description: Build and modify Phoenix LiveView code using current LiveView patterns. Use this skill whenever the user asks for LiveView pages, components, forms, events, navigation, streams, state handling, or LiveView refactors/debugging. Prefer current official Phoenix LiveView docs and examples over memory, then align with local project conventions.
---

# LiveView Idioms

Use this skill when working with Phoenix LiveView.

The goal is to avoid generic frontend habits and instead use current LiveView-native patterns that fit the project's existing code.

## Core workflow

Before making non-trivial LiveView changes:

1. Read `references/docs.md`.
2. Identify the exact LiveView feature involved.
3. Prefer current official docs and examples over memory.
4. Inspect the local codebase for existing LiveViews, components, forms, and event handling patterns.
5. Implement using LiveView-native approaches before reaching for custom JavaScript.

## What to prioritize

Prefer:
- current official Phoenix LiveView docs
- package docs on HexDocs for Phoenix and related libraries
- existing local patterns for components, assigns, forms, streams, navigation, and templates

## Heuristics

- Prefer LiveView-native solutions before adding custom JS.
- Keep state and assigns intentional rather than sprawling.
- Match existing project patterns for components and template structure.
- Check the docs when event handling, forms, streams, uploads, or navigation behavior is involved.
- Do not assume remembered APIs are current.

## Deliverables

When relevant, briefly mention:
- which docs or examples informed the implementation
- which local project pattern you matched

Keep the explanation short and practical.
