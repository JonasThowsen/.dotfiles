# Elixir deslop reference

Apply this after reading the project's Elixir version, dependencies, local conventions, and the `elixir-idioms` skill. Framework-native conventions beat generic advice.

## Data transformation

Prefer `Enum`/`Stream` functions that state the operation over a hand-built `Enum.reduce/3`.

Common simplifications:

- prepend during a real fold and reverse once, rather than repeatedly append with `++`;
- use `Enum.map`, `filter`, `reject`, `flat_map`, `find`, `any?`, `all?`, `group_by`, or `frequencies` when they exactly match the job;
- use `Map.new/2`, `Map.update/4`, `Map.get_and_update/3`, or `update_in` when they make a map transformation direct;
- use `Stream` only when laziness, composition with an eventual consumer, or bounded memory matters.

Keep `reduce` when the accumulator represents a genuine aggregate, state machine, or useful single-pass result. Do not turn a readable reduce into several traversals for cosmetic purity.

## Pipelines

Pipelines should show a value moving through transformations.

Flag:

- pipelines that mix pure transforms, persistence, logging, and notifications with no visible boundary;
- `then/2` used repeatedly to force APIs whose data argument is in the wrong position;
- one-use helpers extracted only to make a pipeline continue;
- `tap/2` hiding important effects;
- long pipelines whose branching or failure behavior is easier to see in `case` or `with`.

A short non-pipelined expression is often clearer than pipeline theater. A long pipeline is fine when every stage is a cohesive, well-named transformation.

## Pattern matching and control flow

Prefer function clauses and pattern matching when variants are part of the function's contract. Prefer `with` for a linear happy path of matching results.

Watch for:

- nested `case` expressions that a focused `with` would flatten;
- a `with` whose large `else` block reconstructs all the branching it claimed to remove;
- catch-all `_` clauses that erase meaningful variants or hide contract drift;
- boolean ladders where tagged tuples or structs would expose the state;
- many tiny clauses that obscure one simple decision.

Do not mechanically convert every conditional to pattern matching. Guards and `if` are idiomatic for genuinely boolean decisions.

## Internal shapes and boundaries

Normalize external data once. Inside trusted code, commit to one shape.

Flag:

- supporting both atom and string keys deep inside the application;
- accepting either a struct or arbitrary map without a real boundary reason;
- repeated `Map.get` defaults after changesets, schemas, or constructors already establish the field;
- repeated nil checks for values guaranteed by a matched struct or validated input;
- converting structs to maps merely to avoid using their public API;
- creating atoms from untrusted strings.

Use structs, changesets, Ash resources, embedded schemas, or explicit constructors where the project already uses them. Do not introduce Ecto or Ash solely to type one local map.

## Errors

Preserve the project's established `{:ok, value} | {:error, reason}` shapes and bang/non-bang conventions.

Flag:

- broad `rescue` or `catch` used as ordinary control flow;
- rescuing an exception only to return an unrelated default;
- flattening distinct errors into `{:error, :failed}` without a boundary reason;
- wrapping infallible pure helpers in `{:ok, value}` just for pipeline uniformity;
- matching every failure as `{:error, _}` when callers need the reason;
- using bang functions on expected invalid input, or replacing deliberate bang semantics during cleanup.

Do not change public error atoms, exception types, or failure timing as deslop work.

## Processes and OTP

A process should own state, isolation, lifecycle, or concurrency. OTP is not a generic service-class pattern.

Flag:

- a `GenServer` that performs a stateless calculation;
- an `Agent` used as a local accumulator;
- a `Task` immediately awaited for work that gains no concurrency or isolation;
- dynamic supervisors, registries, or PubSub added for one fixed caller;
- calls serialized through one process despite independent pure work;
- process dictionary use for ordinary data flow.

Keep processes that own durable runtime state, coordinate resources, isolate failure, enforce ordering, or fit an existing OTP boundary. Preserve supervision and message-order semantics.

## Modules, behaviours, protocols, and macros

Prefer plain functions and data until runtime polymorphism or compile-time generation is real.

Potential slop:

- a behaviour with one implementation and no test/config/runtime substitution need;
- a protocol for a closed set of project-owned structs better handled by explicit functions;
- `use`/`__using__` macros that inject ordinary helpers or hide dependencies;
- `defdelegate` layers that expose the same API without reducing caller knowledge;
- `Utils`, `Helpers`, or `Manager` modules collecting unrelated functions;
- aliases, imports, and module attributes left from abandoned generated code.

A one-implementation behaviour can still be justified at a real external boundary or by an established test adapter. A one-caller module can still be deep and cohesive. Judge the boundary, not a count alone.

## Ash, Ecto, Phoenix, and LiveView

When the project uses a framework, prefer its native mechanism over parallel handwritten infrastructure.

Examples to investigate:

- custom service layers duplicating Ash actions, changes, validations, policies, calculations, or code interfaces;
- manual Ecto struct/map manipulation bypassing established changesets or queries;
- LiveView assigns copied into redundant state instead of derived from authoritative assigns;
- custom JavaScript or message plumbing where LiveView already has an established primitive;
- framework callbacks wrapped in generic indirection that hides lifecycle ownership.

Read the relevant local framework skill and current official docs before changing any of these. Framework code that looks verbose may encode lifecycle or boundary semantics.

## Comments, specs, and tests

Remove comments that restate a pipeline stage or pattern match. Keep comments explaining OTP ordering, framework lifecycle constraints, non-obvious performance choices, or why the obvious implementation is unsafe.

Do not add `@spec` declarations that merely restate obvious private functions if the project does not use that style. Keep specs that define public contracts, complex unions, callbacks, or Dialyzer-relevant intent.

Prefer ExUnit tests through public functions, actions, LiveViews, or process APIs. Avoid tests of private helper extraction, `:sys.get_state` assertions when public behavior exists, and Mox-style call choreography where a faithful boundary test is practical. Preserve useful process, supervision, and integration tests even when they are less "pure."
