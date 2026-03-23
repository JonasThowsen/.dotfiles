---
name: effect-ts-idioms
description: Write idiomatic modern Effect TypeScript using Effect.gen or clear pipelines, typed error channels, Context and Layer services, and resource-safe runtime boundaries. Use when building or reviewing code with Effect, @effect/platform, Schema, Layers, or Effect-based applications.
compatibility: opencode
---

Use this skill to keep Effect code modern, explicit, and faithful to the library's model instead of falling back to Promise-first habits.

## Mindset

Effect code should make three things obvious from the type and structure:
- success value
- expected failure mode
- required services/environment

Prefer Effect-native modeling over wrapping Promise code and keeping the old design.

## Default style

- Use `Effect.gen` for multi-step workflows, branching, and orchestration.
- Use `pipe(...)` plus combinators for short local transformations.
- Keep effects lazy; describe work first, run it only at the program edge.
- Write explicit lambdas instead of point-free or tacit style.

Prefer:
```ts
Effect.map((user) => formatUser(user))
```

Over:
```ts
Effect.map(formatUser)
```

This follows the official guidance to avoid tacit usage because it can hurt inference, overload resolution, and stack traces.

## Error modeling

- Put expected domain and operational failures in the error channel.
- Reserve defects for truly unexpected bugs or violated invariants.
- Prefer tagged, structured errors over raw strings.
- Recover close to the boundary where a fallback is meaningful.

Good defaults:
- `Effect.fail(...)` for expected failures
- `Effect.try(...)` / `Effect.tryPromise(...)` when wrapping throwing or rejecting APIs
- `catchTag`, `catchAll`, and `mapError` for recovery and translation

Avoid:
- throwing inside `Effect.sync`
- burying business failures in `Option.none()` when the caller needs an explanation
- converting everything to `unknown` or `Error` too early

## Services, Context, and Layers

- Model shared capabilities as services with `Context.Tag`.
- Provide implementations with `Layer` for reusable dependency graphs.
- Keep service interfaces small and capability-oriented.
- Inject services through requirements, not global singletons.

Prefer:
- pure domain functions for pure logic
- service tags for effectful capabilities
- layers at app assembly boundaries

Do not manually thread giant dependency objects through business logic if Effect requirements can model them cleanly.

## Resource safety

- Use scoped resource management for files, sockets, clients, subscriptions, locks, and anything requiring cleanup.
- Prefer `Effect.acquireRelease`, `Effect.addFinalizer`, and scoped layers over manual cleanup bookkeeping.
- Treat interruption as a normal part of execution, not an edge case.

If something must always be cleaned up, model it in the effect structure rather than relying on convention.

## Runtime boundaries

- Run effects at the outermost program boundary.
- In Node apps, prefer `NodeRuntime.runMain(...)` for the main program so shutdown is graceful.
- Avoid `Effect.runPromise` or `Effect.runSync` inside library code, helpers, or domain modules.
- Convert external APIs at the edge, then stay inside Effect.

## Concurrency and composition

- Use fibers and combinators deliberately; choose sequential vs concurrent behavior explicitly.
- Prefer `Effect.all(...)` and concurrency options for independent work.
- Use retries, schedules, timeouts, and supervision as first-class tools instead of ad hoc loops.
- Keep concurrent workflows readable; name intermediate effects when needed.

## Data modeling

- Prefer `Schema` at boundaries for parsing, validation, and encoding.
- Prefer `Option`, `Either`, `Data`, `Chunk`, `Duration`, and other Effect data types when they make semantics clearer.
- Avoid `null` and `undefined` in domain modeling when `Option` or a tagged union is better.

## Interop rules

- Wrap promise-returning APIs with `Effect.tryPromise` unless rejection is impossible.
- Wrap callback or event APIs once, then expose an Effect-native interface.
- Keep framework glue thin.

## Review checklist

When writing or reviewing Effect code, check:
- Is expected failure in the error channel?
- Are services expressed as requirements instead of hidden globals?
- Is resource cleanup guaranteed by structure?
- Is the runtime only used at the edge?
- Is the code using `Effect.gen` or pipelines in a way that is easy to follow?
- Is this still really Effect code, or just Promise code wrapped at the last second?
