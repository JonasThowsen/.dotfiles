---
name: tracer-bullet-development
description: Implement features as the smallest useful end-to-end production slice, using tracer-code/tracer-bullet development to validate architecture through working software. Use this skill whenever planning or implementing a feature, product capability, integration, workflow, or uncertain architectural path—even when the user does not explicitly mention tracer bullets. Prefer a thin vertical slice through real boundaries over building complete horizontal layers first.
---

# Tracer-Bullet Development

Build each feature as a small, observable end-to-end slice before broadening it. The first slice should prove both user value and the architectural path with working software.

A tracer is a production foundation, not a disposable prototype. Keep the implementation narrow, but make the path real enough to reveal integration constraints and inform later design.

## Core rule

For every feature:

1. Define one observable end-to-end behavior.
2. State a concrete acceptance criterion.
3. Identify the full path from the initiating actor or input to the observable result.
4. Implement only the minimum behavior across every necessary boundary.
5. Exercise that complete path in a realistic environment.
6. Use what the tracer reveals to choose the next increment and refactor only demonstrated problems.

Prefer one thin working path over several complete disconnected layers.

## Before implementation

Inspect the project plan, requirements, existing code, and tests. Determine:

- who or what initiates the behavior
- what useful result they must observe
- which boundaries the request must cross, such as UI, API, domain logic, database, queue, external process, deployment, or third-party integration
- the riskiest assumption the slice should test
- what can be deliberately deferred without making the result fake

Briefly state the proposed tracer and acceptance criterion before making substantial changes. If the desired behavior is materially ambiguous, ask a focused question. Otherwise infer the narrowest useful behavior and proceed.

## Choose a vertical slice

The slice should cross the real system from entry point to result. Include only the narrowest case needed to prove the path.

Good slices look like:

- one UI action persisted through the real domain and database, then rendered back to the user
- one deployment request resolved to an immutable revision, built, sent to one target, started, and health-checked
- one external event received, validated, stored, processed, and exposed through an observable status

Avoid horizontal implementation sequences such as:

- designing every domain abstraction before one use case works
- completing all adapters before connecting one real workflow
- building a full UI against mocked behavior while the integration path remains unknown
- handling every variant, role, provider, and edge case in the first increment

Introduce shared abstractions only when the working slice exposes real duplication, coupling, or a required seam.

## Keep the path real

Use real boundaries where they are central to what the tracer must prove. A simulation or fake may establish an initial walking skeleton, but do not keep elaborating the simulation when the next uncertainty is the real integration. Replace one complete simulated path with a narrow real path.

Build on production-quality foundations:

- preserve durable data and state transitions where durability matters
- avoid shell interpolation and insecure shortcuts in execution paths
- use the production artifact or production-style build, not only the development server
- keep configuration and interfaces compatible with intended deployment
- make the tracer testable and maintainable

Temporary shortcuts are acceptable when they make learning faster without invalidating the path. Mark them clearly with a specific follow-up and explain why they are safe and reversible. Do not let an unmarked shortcut silently become architecture.

## Leave deferred work at the boundary

When a tracer deliberately defers functionality at an implemented code boundary, leave an inline `TODO(tracer)` comment at that exact location. The comment should state what is deferred, why the current narrow behavior is acceptable, and the condition or next slice that should replace it.

Prefer actionable comments such as:

```text
TODO(tracer): Replace global deployment serialization with PostgreSQL per-target
leases before allowing more than one deployment worker.
```

Avoid vague comments such as `TODO: improve this`. Keep broader features with no meaningful code location in the project roadmap or backlog instead of attaching them to unrelated code. Remove an inline tracer TODO only when the deferred behavior is implemented, deliberately rejected, or its boundary no longer exists.

## Implement the minimum complete behavior

Connect only the components needed for the acceptance criterion. Within the slice:

- keep domain rules explicit and testable
- implement pure transformations before effects where practical
- pass dependencies explicitly at uncertain or external boundaries
- preserve observable progress and failures
- handle the important failure mode that would invalidate the tracer
- defer optional breadth, customization, and polish

Do not confuse “thin” with “partial.” A thin slice still reaches the observable end result.

## Test and evaluate

Use tests to protect what the tracer proves:

- focused tests for domain invariants
- integration tests at failure-prone boundaries
- an end-to-end or deployed-style check for the acceptance criterion

Exercise the actual workflow after implementation. When relevant, build the production artifact and run it in the environment closest to real use that is reasonably available.

A feature is not demonstrated merely because each layer compiles independently.

## Learn before broadening

After the tracer works:

1. Record what was proven.
2. Record assumptions, deferred cases, and marked shortcuts.
3. Note architecture or product decisions revealed by real use.
4. Refactor only coupling or accidental complexity now visible.
5. Select the next smallest useful behavior rather than filling layers speculatively.

Expand edge cases and capabilities incrementally while keeping the end-to-end path working.

## Completion report

When reporting completed work, include:

- the observable behavior now working
- how the end-to-end path was validated
- tests or production-style checks run
- deliberate deferrals and marked shortcuts
- the next natural tracer slice
