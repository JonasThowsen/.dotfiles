# Functional-programming deslop reference

Functional cleanup should reduce accidental state and control flow while keeping intent obvious. It is not a purity contest.

## Pure core, effectful boundary

Prefer pure functions for decisions and transformations. Let a small boundary own I/O, clocks, randomness, configuration, persistence, and process interaction.

Flag:

- a helper that reads global configuration while transforming one value;
- logging or persistence buried inside mapping/filtering callbacks;
- mutable state or an actor used only as an accumulator for one synchronous calculation;
- callers forced to construct an effectful service for logic that only needs values.

Do not flag:

- effects at an application boundary;
- state that must survive calls or coordinate independent work;
- local mutation hidden behind a pure interface when the language uses it for a measured or standard implementation;
- deliberate streaming where eagerness would change resource use.

## Use the narrowest transformation

Choose a combinator that names the operation:

- map for one output per input;
- filter/reject for selection;
- flat-map for zero or more outputs;
- find/any/all for a short-circuiting question;
- group/index functions when the standard library already owns grouping;
- reduce/fold when the result genuinely carries state across elements.

A fold is suspicious when it manually rebuilds a standard map/filter/group operation, mutates several unrelated accumulators, or encodes a state machine nobody named. A fold is appropriate for a real aggregate, a single-pass multi-result computation, or explicit state transition.

Prefer one clear pass over both extremes: repeated wasteful traversals and one giant fold optimized before evidence exists.

## Model valid states directly

Prefer tagged variants, algebraic data types, structs/records, and pattern matching over loose maps plus boolean combinations.

Flag:

- flags whose combinations create impossible states;
- strings or magic atoms standing in for a closed domain;
- catch-all patterns that hide a newly added variant;
- internal values repeatedly checked for shape because no boundary normalized them;
- parallel optional fields that are meaningful only together.

Do not replace a stable local representation solely to demonstrate a more sophisticated type. The migration cost must be in scope and justified.

## Pattern matching should expose decisions

Use function clauses, `case`/match expressions, and destructuring when they make the domain branches explicit. Avoid scattering the same shape test across leaves of the call tree.

Flag both:

- deeply nested conditionals that should be named variants or matched clauses;
- clever point-free or combinator-heavy code that conceals important branches, errors, or effect order.

The target is visible control flow, not the fewest tokens.

## Keep abstractions earned

Functional code can over-engineer through tiny helpers, type classes/traits/behaviours, free-style effect layers, generic folds, and callback injection just as easily as object-oriented code can over-engineer through classes.

Ask:

1. Does this abstraction have more than one real use or implementation?
2. Does it hide detail while exposing a smaller, more stable concept?
3. Does it reduce caller burden?
4. Is the indirection conventional in this repository?

If not, keep the operation concrete and local. Conversely, do not inline a cohesive module merely because it has one current caller; a deep module can earn its boundary by hiding substantial complexity.

## Make effects and dependencies explicit

Passing values and capability-specific functions is often clearer than passing a whole environment, context, or service locator. Prefer the smallest dependency a function needs.

Avoid threading a large context through every pure helper. Normalize external data once, then pass domain values inward. Return decisions or effect descriptions outward when that is already the project's architecture; do not invent an effect system for a local cleanup.

## Preserve error semantics

Cleanup must preserve which failures are values, which are exceptions, when they occur, and how much context they carry.

Flag:

- broad catches that silently return neutral values;
- one generic error replacing distinct domain variants;
- exceptions used as ordinary branching where the language convention uses result values;
- result wrappers added around functions that cannot fail;
- fallback chains compensating for an unknown contract.

Never "improve" a public error shape during deslop. Report it as separate behavior-changing work.

## Concurrency must own a real concern

Concurrency is justified by independent work, isolation, state ownership, cancellation, latency, or measured throughput. It is not justified merely because the language makes spawning easy.

Check for:

- concurrent work immediately serialized through shared state;
- tasks spawned for trivial pure operations;
- unbounded traversal;
- an actor/process that stores no durable state and receives one synchronous request;
- changed ordering, cancellation, or error propagation hidden by a cleanup.

Prefer the simplest sequential pure transformation until a real concurrency requirement is established.

## Tests should observe behavior

Prefer examples and properties through public boundaries. Pure functions often make tests cheap, but cheap tests are not automatically useful.

Flag tests that mirror the implementation's fold, assert only mock calls, target extracted private helpers, or duplicate cases without representing distinct domain behavior. Keep tests whose examples document a law, boundary, regression, or important variant.
