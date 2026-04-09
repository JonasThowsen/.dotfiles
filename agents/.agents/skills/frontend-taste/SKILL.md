---
name: frontend-taste
description: Design and implement tasteful, modern frontend UI with strong layout discipline, mobile-first composition, and clean CSS. Use this skill whenever the user asks for a page, component, layout, redesign, styling pass, responsive improvements, or frontend polish. Prefer plain CSS unless the project clearly already uses Tailwind CSS. Consult the bundled references for modern CSS guidance and local design inspiration before producing UI code.
---

# Frontend Taste

Use this skill when building or redesigning frontend interfaces.

The goal is not flashy AI-looking UI. The goal is confident, restrained, well-composed interfaces with strong spacing, clean hierarchy, and layouts that feel intentional on mobile first.

## Core approach

Before writing code:

1. Check what styling system the project already uses.
2. If the project clearly uses Tailwind CSS, continue with Tailwind.
3. Otherwise, prefer plain CSS.
4. Read the relevant reference files:
   - `references/modern-css.md`
   - `references/inspiration.md`
5. If needed, consult recent Mozilla / MDN CSS documentation for modern best practices and current CSS behavior.

Do not force a styling system that the project is not already using.

## Styling system choice

### Prefer plain CSS

Default to plain CSS when possible.

That means:
- CSS modules, component CSS, or stylesheet files are usually preferred over utility-heavy markup
- keep class names semantic and readable
- express layout and visual hierarchy in CSS rather than in long utility class strings

### Use Tailwind only when the project already uses it

If the repository clearly uses Tailwind CSS already, match the existing approach.

When using Tailwind:
- still follow the layout and visual principles in this skill
- use local inspiration and translate the design taste, not just the exact utility classes
- avoid noisy class soup when a simpler structure would do

### Convert Tailwind-inspired ideas into plain CSS when needed

The local inspiration library may contain many Tailwind examples. Treat them as design references, not implementation constraints.

If the project is not already using Tailwind:
- extract the layout, spacing, rhythm, and typography ideas
- implement them in plain CSS
- do not introduce Tailwind just because the inspiration source used it

## Layout principles

### Start with layout, not decoration

Solve the structure first:
- define the main regions
- choose the right layout model
- create clear rhythm through spacing
- only then add visual treatments

### Prefer grid-template-areas for page and section layout

For larger compositions, use CSS Grid with named areas so the structure is obvious and maintainable.

Use `grid-template-areas` whenever it makes placement clearer.

Good uses:
- page shells
- hero layouts
- dashboard regions
- marketing sections with multiple content zones
- responsive rearrangement of named content blocks

### Use flexbox when grid is not the right tool

Use Flexbox for:
- one-dimensional alignment
- button rows
- nav bars
- small stacked groups
- inline metadata clusters

Do not use Grid and Flexbox arbitrarily. Pick the simpler layout model that matches the problem.

## Visual design principles

### Favor solid colors over gradient-heavy design

Do not lean on gradients as the main design device.

Fine:
- flat backgrounds
- restrained color blocks
- subtle tonal shifts
- occasional accent gradients if there is a strong reason

Avoid:
- gradient-everywhere styling
- glossy, generic AI-looking backgrounds
- decoration that compensates for weak layout

### Use spacing as the primary grouping mechanism

Prefer spacing over boxes.

Start with more spacing than you think you need, then reduce it if the layout feels too loose.

Use whitespace to establish:
- section separation
- reading rhythm
- hierarchy
- visual calm

### Avoid cards unless they are truly appropriate

Do not default to card grids.

Cards are acceptable when the content is genuinely card-like, such as:
- product listings
- repeated item collections
- dashboards with distinct independent widgets
- previews where containment helps comprehension

If the content can be structured with typography, spacing, and layout alone, prefer that.

### Do not use borders everywhere

Avoid using borders as the default separator for every region.

Instead prefer:
- spacing
- background tone changes
- subtle box shadows
- color/hue shifts
- typographic hierarchy

If a border is used, it should have a clear reason.

## Mobile-first UX

Always prioritize mobile as the main UX driver.

Assume most people will first experience the interface on a phone. The mobile version should not be a reduced afterthought; it should be the primary composition.

This means:
- start with a mobile layout first
- ensure comfortable spacing and touch targets
- keep line lengths and text sizes readable on small screens
- avoid cramped multi-column patterns on narrow viewports
- let desktop enhancements grow naturally from the mobile structure

Ask of every layout:
- does this feel calm and legible on a phone?
- are the major actions easy to find and tap?
- does the spacing still breathe on a small screen?

## Typography and hierarchy

Typography is often the difference between a UI that feels designed and one that feels generic.

Create hierarchy through:
- spacing
- scale
- weight
- line-height
- measure and readable line length
- color contrast
- grouping

Prefer interfaces that feel editorial, composed, and readable.

Use typography deliberately:
- keep the number of text sizes limited and intentional
- give headings enough contrast from body copy to create structure
- use line-height generously, especially on mobile
- avoid cramped text blocks and tiny supporting text
- let spacing and type scale do more of the hierarchy work than boxes and dividers

Do not rely on excessive decoration to signal importance.

## Implementation workflow

When producing frontend code:

1. Inspect the project to determine whether it uses plain CSS or Tailwind CSS.
2. Read `references/inspiration.md` and inspect the local inspiration path it points to if visual direction is needed.
3. Read `references/modern-css.md` and, when needed, consult recent MDN CSS docs.
4. Sketch the layout mentally in terms of named regions.
5. Build the mobile layout first.
6. Add desktop rearrangement with Grid areas or other deliberate layout changes.
7. Keep visual separation primarily driven by spacing, color, and depth rather than borders and cards.
8. Use gradients sparingly.

## What to avoid

Avoid these common failure modes:
- generic startup landing-page aesthetics
- too many gradients
- card-on-card-on-card composition
- over-bordered sections
- desktop-first layouts awkwardly squeezed onto mobile
- utility spam in non-Tailwind projects
- weak spacing rhythm
- using decoration to hide poor structure

## Design review checklist

Before finishing, sanity-check the result:
- does the mobile layout feel like the primary layout rather than a compressed desktop version?
- is the composition strong without relying on gradients?
- are sections separated mostly by spacing, tone, and depth rather than borders?
- did cards appear only where they are genuinely useful?
- is Grid using named areas where that improves clarity?
- is Flexbox used only for one-dimensional alignment problems?
- does the typography feel calm, readable, and intentionally scaled?
- is there slightly more spacing than a typical default UI would use?
- does the interface still look good before decorative effects are added?

If the answer to several of these is no, revise the layout before polishing details.

## Deliverables

When relevant, explain the design direction briefly in practical terms, for example:
- why Grid or Flexbox was chosen
- how spacing is doing the grouping work
- how the mobile layout drives the rest of the design
- how typography is carrying hierarchy

Keep explanations short and implementation-focused.
