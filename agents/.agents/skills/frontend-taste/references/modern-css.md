# Modern CSS reference

Read this file when doing frontend styling work, especially before making layout choices.

## Purpose

Use modern, current CSS patterns instead of stale habits.

When there is any uncertainty about behavior, syntax, browser support, or best practice, consult recent Mozilla / MDN CSS documentation before finalizing implementation.

## Guidance

Prioritize:
- mobile-first responsive design
- CSS Grid for two-dimensional layout
- `grid-template-areas` when named placement improves clarity
- Flexbox for one-dimensional alignment and distribution
- logical, semantic class names in plain CSS projects
- modern spacing scales using `gap`, padding, and margins intentionally
- fluid but restrained sizing where appropriate
- accessible, readable typography and touch targets

Avoid outdated habits such as:
- using layout hacks where Grid or Flexbox solves the problem directly
- relying on borders for all separation
- over-nesting wrappers when a cleaner structure would work
- treating mobile as an afterthought

## MDN topics worth checking

If you need confirmation or a refresher, check recent MDN docs for topics like:
- CSS Grid Layout
- `grid-template-areas`
- Flexbox
- `gap`
- media queries
- modern viewport units
- container queries if relevant to the project
- logical properties when useful
- typography and accessibility-related CSS guidance

## Rule of thumb

If you are about to use a complicated workaround, pause and check whether modern CSS has a cleaner built-in solution.
