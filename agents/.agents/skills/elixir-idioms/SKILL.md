---
name: elixir-idioms
description: Write idiomatic Elixir and Phoenix code with pattern matching, small composable functions, OTP-friendly boundaries, and BEAM-aware error handling. Use when creating or reviewing Elixir code, APIs, GenServers, Phoenix apps, Ecto code, or tests.
compatibility: opencode
---

Use this skill to keep Elixir code assertive, readable, and aligned with core language and OTP conventions.

## Mindset

Prefer:
- Data transformation over object-style mutation
- Pattern matching over manual branching
- Small focused functions over large control-flow blocks
- Explicit return shapes over implicit conventions
- Supervision and process isolation over defensive over-catching

Write code that feels natural to experienced Elixir developers, not translated from Ruby, JavaScript, or OO languages.

## Core style rules

- Use descriptive module and function names so comments are rarely needed.
- Keep public APIs small and intention-revealing.
- Use pipelines when each step clearly transforms the previous value.
- Do not force everything into a pipeline; break out named variables when branching or meaning improves.
- Prefer multi-clause functions when clauses represent genuinely different shapes or cases.
- Keep clauses related; do not use multi-clause functions as a grab bag.

## Pattern matching and control flow

Prefer assertive code:
- Match on expected shapes directly.
- Let invalid shapes fail early unless the caller truly needs an error tuple.
- Use guards for real invariants, not as a substitute for better data modeling.

Prefer:
- `case` for branching on a value
- `with` for linear happy-path flows that short-circuit on failure
- multiple function clauses for input shape dispatch

Avoid:
- giant `cond` or `case` blocks where private helper functions would clarify intent
- complex `with ... else` blocks that normalize many unrelated error shapes in one place

If `with` needs a complicated `else`, push normalization closer to the source via small private helpers.

If `with` needs a lot of different error handlers, use private helpers to handle the error handling, keeping the `with` tidy.

## Data shapes and APIs

- Use structs when data has a stable shape shared across modules.
- Use maps for ad hoc or boundary-shaped data.
- Use keyword lists mainly for optional arguments and configuration.
- Prefer tagged tuples like `{:ok, value}` / `{:error, reason}` for expected operational failures.
- Raise for programmer errors, impossible states, or misuse of internal APIs.

For maps and structs:
- Use `map.key` when the key must exist.
- Use `map[:key]` only when the key is optional or dynamic.

## Elixir-specific anti-patterns to avoid

- Do not create atoms from untrusted input with `String.to_atom/1`.
- Do not over-comment self-explanatory code.
- Do not hide invalid data with non-assertive pattern matching.
- Do not return vague catch-all values when the failure mode matters.
- Do not pass long unrelated parameter lists when a map/struct would clarify the API.

## OTP and process boundaries

When working with processes:
- Put stateful behavior behind a clear process boundary.
- Keep GenServers thin; move domain logic into plain modules when possible.
- Store minimal necessary state.
- Model sync vs async calls intentionally.
- Let supervisors own lifecycle and recovery concerns.

Avoid turning GenServers into generic service objects or dumping all business logic into callbacks.

## Phoenix and Ecto conventions

In Phoenix code:
- Keep controllers, LiveViews, and channels thin.
- Push domain logic into contexts or dedicated modules.
- Validate and cast external input explicitly.
- Prefer explicit assigns and event names.

In Ecto code:
- Use changesets for external data validation and casting.
- Keep schema modules focused on schema and changeset concerns.
- Prefer composable query helpers over giant inline queries.
- Make preload behavior intentional.

## Naming and documentation

- Use `!` only for the raising variant of an existing safe operation.
- Name predicates with `?`.
- Name bangless versions as the default safe API when both forms exist.
- Use `@moduledoc` and `@doc` for public modules and public functions when it helps users of the API.

## Testing

- Prefer ExUnit tests that read like executable examples.
- Test public behavior first, private helpers indirectly.
- Pattern match in assertions when shape matters.
- Use factories/builders that keep test data explicit enough to read.

## Review checklist

When writing or reviewing Elixir code, check:
- Is the data shape clear?
- Are expected failures modeled explicitly?
- Is pattern matching doing the heavy lifting?
- Are functions short and well named?
- Is OTP used only where state/concurrency/lifecycle actually matter?
- Does this look like idiomatic Elixir instead of another language wearing Elixir syntax?
