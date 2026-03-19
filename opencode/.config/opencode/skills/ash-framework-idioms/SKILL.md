---
name: ash-framework-idioms
description: Write idiomatic Ash framework code using intentful actions, code interfaces, relationship management, DSL-first modeling, and framework-native changes/preparations instead of ad hoc helper layers. Use when creating or reviewing Ash resources, domains, actions, queries, or Phoenix integrations.
compatibility: opencode
---

Use this skill to keep Ash code aligned with the framework's intended style instead of recreating Ecto-context patterns or building custom wrapper layers around features Ash already provides.

## Core mindset

Ash applications should express domain behavior in:
- resources
- actions
- relationships
- calculations and aggregates
- changes, validations, and preparations
- code interfaces

Prefer Ash-native DSL and action modeling over hand-written helper modules that assemble queries and changesets everywhere.

## Primary rules

- Put business behavior in well-named actions.
- Use code interfaces for function-style access from the rest of the app.
- Keep controllers, LiveViews, and other web modules thin.
- Prefer resource capabilities over bespoke helper functions.
- Reach for `Ash.Query` / `Ash.Changeset` directly only when customizing invocation of an already meaningful action, or when building framework-level tooling.

## Actions over helpers

Ash is not meant to stop at generic CRUD.

Prefer:
- many small, intentful actions
- `:open`, `:archive`, `:top`, `:assign_reviewer`, `:publish`, `:approve`

Over:
- one generic `:read`, `:update`, `:destroy` plus a pile of helper functions in contexts or LiveViews

Bad shape:
- web layer builds filters, sorting, limits, and domain rules manually every time

Better shape:
- define a named action that captures the behavior
- expose it via a code interface
- optionally customize the query when needed at the call site

If you see a helper like `top_tickets(user_id)` that mostly builds an Ash query, strongly consider making it a read action plus a code interface instead.

## Code interfaces are the default app-facing API

When Ash resources/actions are called from application code, prefer code interfaces on the domain or resource.

Use them for:
- create/update/destroy flows
- `get_by` accessors
- calculation exposure
- authorization checks

Prefer:
- `MyApp.Support.open_ticket(subject)`
- `MyApp.Dashboards.get_dashboard_group_by_id!(id, load: [...])`

Over:
- calling `Ash.get!`, `Ash.read!`, `Ash.create!`, etc. directly from controllers, LiveViews, or unrelated modules unless the module is intentionally composing/customizing the action call

Important guidance:
- use `get_by` code interfaces instead of sprinkling `Ash.get!` in web modules
- use generated `can_*?` functions for UI authorization checks when relevant
- use `default_options` sparingly for stable interface defaults

## DSL-first modeling

Ask before writing helper code: "Does Ash already have a way to model this?"

Usually prefer Ash features like:
- action arguments
- `accept` / `default_accept`
- validations
- changes
- preparations
- calculations
- aggregates
- identities
- relationships and `manage_relationship`
- policies

Avoid writing boilerplate conversion or orchestration code if the DSL can describe it directly.

## Use the right extension point

- **Changes** customize create/update/destroy behavior.
- **Preparations** customize read and generic action behavior.
- **Validations** enforce business rules.
- **Calculations** expose derived values.
- **Aggregates** expose counts/sums/etc.
- **Relationships** model traversal and related-data management.

Do not put read-query logic into custom helper modules when a preparation on a read action is the right home.
Do not put write-side orchestration into controllers when a change on the action is the right home.

## Relationship management

Prefer `manage_relationship` when the intent is standard relationship CRUD from action input.

Use `change manage_relationship(...)` when:
- input comes from action arguments
- you want AshPhoenix/AshGraphQL/AshJsonApi to understand the behavior naturally

Use `Ash.Changeset.manage_relationship/4` directly when:
- building related values programmatically in a custom change
- the flow is conditional or transformed before relationship management

Avoid manually updating foreign keys and join records unless there is a genuinely special case.

## Keep web layers thin

In Phoenix or LiveView code:
- call domain/resource code interfaces
- pass action args and options
- keep filtering, loading, and domain semantics in the Ash layer

Do not treat Ash like a glorified Repo wrapper.

Bad pattern:
- controller or LiveView manually builds query/filter/sort/load chains that represent domain behavior

Good pattern:
- named resource action + code interface, with optional query customization only when truly needed

## Defaults are for bootstrapping, not the end state

`defaults [...]` is a good starting point, but mature Ash resources often need more expressive action sets.

Do not stop at generic CRUD if the resource has meaningful domain operations.

## Custom changes and preparations

- Anonymous functions are fine for prototyping.
- Prefer modules for lasting behavior.
- Module-based changes/preparations are more reusable, more organized, and support advanced features like atomic or batch behavior.

If behavior matters enough to keep, give it a proper module.

## Nested actions and context

When invoking nested actions from changes or preparations:
- preserve scope/context properly
- prefer `scope: context` or the relevant Ash helpers for threading shared context

Do not manually drop important actor/tenant/shared information.

## Query customization guidance

It is okay to customize invocation outside the action when necessary, for example:
- adding an extra filter to a named read action
- adding dynamic loads
- providing tenant/actor/context options

But the base meaning should still live in the action itself.

Think:
- actions define the capability
- code interfaces expose the capability ergonomically
- ad hoc query building refines the capability for a specific call site

## Review checklist

When writing or reviewing Ash code, check:
- Should this helper actually be an action?
- Should this direct `Ash.*` call be a code interface call instead?
- Is query logic living in a preparation or named read action where it belongs?
- Is write logic living in changes/validations/manage_relationship where it belongs?
- Is the web layer too aware of Ash internals?
- Is this recreating framework behavior that Ash already provides?

## Good outcomes

Idiomatic Ash code tends to have:
- thin web modules
- rich, named actions
- clear code interfaces
- less boilerplate helper code
- fewer manual query pipelines scattered across the app
- behavior described declaratively in the resource/domain DSL
