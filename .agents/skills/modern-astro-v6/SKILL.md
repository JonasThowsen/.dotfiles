---
name: modern-astro-v6
description: Build idiomatic Astro v6 applications with server-first rendering, islands, content collections, actions, and current v6-safe APIs. Use when creating, reviewing, or migrating Astro projects, pages, components, content layers, or integrations.
compatibility: opencode
---

Use this skill to keep Astro work aligned with modern v6 practices and away from legacy patterns.

## Core mindset

Astro is server-first and HTML-first.

Default to:
- static or server-rendered HTML
- minimal client JavaScript
- islands only where interactivity or personalization is needed
- content and data APIs that preserve type safety

Do not design Astro apps like full-SPA apps unless the product truly needs it.

## Rendering defaults

- Keep pages and Astro components server-rendered by default.
- Add `client:*` directives only to components that need browser interactivity.
- Choose hydration directives intentionally:
  - `client:load` for immediately interactive UI
  - `client:idle` for lower-priority interactions
  - `client:visible` for below-the-fold or deferred UI
- Use `server:defer` for expensive or personalized server islands that should not block the whole page.

Avoid hydrating large layout regions when one small widget would do.

## Modern v6 APIs and constraints

- Use `astro/zod` for Zod imports.
- Use content collections and loaders instead of legacy content APIs.
- Do not use `Astro.glob()`; it is removed in v6.
- Do not import `z` from `astro:content` or `astro:schema`; use `astro/zod`.
- Use ESM config files; CommonJS config is removed.

Upgrade-aware defaults:
- Node `22.12.0` or higher
- Vite 7-compatible assumptions
- Zod 4-compatible schemas and validators

## Page and component architecture

- Put data fetching in frontmatter/server code by default.
- Keep Astro components mostly presentational and server-friendly.
- Use framework components for interactive islands, not as the default for everything.
- Pass plain serializable props across the Astro/framework boundary when possible.

If a page can be mostly static HTML, keep it that way.

## Content strategy

- Use `src/content.config.ts` to define collections.
- Prefer build-time collections for content that is mostly static.
- Use live collections only when runtime freshness is worth the performance tradeoff.
- Define schemas for collections whenever possible.
- Sort `getCollection()` results explicitly; order is not guaranteed.

Good modern content setup usually means:
- `defineCollection(...)`
- a loader such as `glob()` or `file()`
- `schema: z.object(...)`

## Actions and forms

- Prefer Astro Actions over bespoke API endpoints when the goal is typed server functions for UI interactions.
- Define actions in `src/actions/index.ts`.
- Validate inputs with `astro/zod`.
- Check authorization in the action handler or middleware as appropriate.
- Use `Astro.getActionResult()` for no-JS form flows and server-side result handling.

Remember that actions are public endpoints and need the same auth discipline as API routes.

## Images, assets, and styling

- Prefer Astro asset handling and image optimization when available.
- Use CSS that supports a fast initial render; avoid requiring client JS for layout.
- Build responsive layouts that work well without hydration.

## Routing and data decisions

Choose the simplest tool that fits:
- page frontmatter for normal page data needs
- content collections for structured content
- actions for typed UI-to-server mutations
- endpoints when you truly need a custom HTTP surface
- server islands when only a fragment needs dynamic server rendering

## Migration guardrails for v6

When touching older code, look for and modernize these patterns:
- `Astro.glob()` usage
- `astro:schema` imports
- `z` imported from `astro:content`
- old content collection compatibility assumptions
- CommonJS Astro config
- `import.meta.env.ASSETS_PREFIX` usage where `astro:config/server` is more appropriate

Also be aware that `getStaticPaths()` should not rely on the old `Astro` object; prefer `import.meta.env.SITE` when needed.

## Output expectations

When implementing or reviewing Astro code, optimize for:
- less shipped JavaScript
- clean server/client boundaries
- upgrade-safe v6 APIs
- content and form flows with strong typing
- simple, maintainable project structure
