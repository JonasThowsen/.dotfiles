---
name: ui-ux
description: Design product interfaces and UX flows with strong usability, hierarchy, accessibility, and implementation-aware tradeoffs. Use when planning or refining UI, UX, information architecture, wireframes, flows, forms, navigation, or interaction details.
compatibility: opencode
---

Use this skill when the work is primarily about making an interface easier to understand, easier to use, or more effective.

Default mindset:
- Start from user goals, not screens.
- Prefer clarity over novelty, but avoid blandness.
- Make important actions obvious.
- Reduce cognitive load before adding features.
- Treat copy, states, and accessibility as core UI work, not polish.

## First principles

Always reason through these in order:
1. Who is the primary user?
2. What is the main job they are trying to complete?
3. What is the highest-risk moment in the flow?
4. What information or action deserves the most visual emphasis?
5. What would make this feel trustworthy, fast, and low-friction?

If context is missing, infer likely user goals from the product and existing code before asking questions.

## Core heuristics

Apply these by default:
- Clear hierarchy: one primary action, a small number of secondary actions, and obvious grouping.
- Progressive disclosure: hide advanced options until they are needed.
- Strong affordances: interactive things must look interactive.
- Recognition over recall: show choices, labels, previews, and examples instead of making users remember data.
- Fast feedback: loading, success, validation, and error states must be immediate and specific.
- Reversible actions: support undo, confirmation, or safe recovery for destructive paths.
- Consistency: repeated patterns should behave the same way across screens.
- Accessible defaults: keyboard access, visible focus, sufficient contrast, semantic structure, and descriptive labels.

## Information architecture

When shaping pages, forms, dashboards, or navigation:
- Group by user intent, not by backend model shape.
- Put the most common task first.
- Move rarely used or dangerous actions away from the primary path.
- Use section titles that answer "what can I do here?"
- Prefer fewer choices with better labels over many loosely differentiated options.

For navigation:
- Keep top-level navigation stable and short.
- Avoid ambiguous labels like "Manage", "General", or "Other" unless the product already uses them consistently.
- Make current location, next step, and escape hatches obvious.

## Interaction design

For every interactive surface, define:
- Default state
- Hover/focus/pressed states
- Empty state
- Loading state
- Validation/error state
- Success/confirmation state
- Disabled state, if applicable

For forms:
- Ask only for what is necessary.
- Prefer inline help over long instructional paragraphs.
- Put examples inside labels or helper text when format matters.
- Validate close to the field and explain how to fix the problem.
- Use sensible defaults and autofill-friendly input choices.

For tables and dense UIs:
- Preserve scanability with alignment, spacing, and predictable action placement.
- Keep bulk actions separate from row actions.
- Make sorting, filtering, and selection states explicit.

## Visual design guidance

When suggesting or implementing visuals:
- Create a deliberate visual hierarchy with spacing, type scale, and contrast.
- Use color primarily to communicate meaning and emphasis, not decoration alone.
- Reserve strong accent color for the most important actions or statuses.
- Prefer fewer, stronger visual ideas over many weak ones.
- Design mobile and desktop intentionally; do not just stack everything.

Avoid:
- Walls of equal-weight content
- More than one primary CTA in the same region
- Low-contrast placeholder-heavy forms
- Icon-only actions without enough context
- Generic dashboard clutter

## UX writing

Use copy that is:
- Short
- Specific
- Actionable
- Human

Prefer:
- "Save changes" over "Submit"
- "Invite teammate" over "Add"
- "Enter a valid email address" over "Invalid input"

Avoid vague microcopy, passive voice, and internal jargon unless the existing product clearly depends on it.

## Accessibility baseline

Never ship a UI concept that ignores:
- Keyboard navigation
- Focus visibility
- Label-input association
- Error text that does not rely on color alone
- Tap targets large enough for touch
- Responsive layouts that work at narrow widths

If changing markup or components, preserve semantics and screen-reader usefulness.

## Working style

When asked to improve or create UI/UX:
- Diagnose the current friction first.
- State the primary UX goal in one sentence.
- Make the smallest set of structural changes that meaningfully improves the flow.
- Preserve existing design systems and product conventions unless the task is explicitly exploratory.
- If implementing code, ensure the design remains practical to build and maintain.

## Output expectations

When presenting work, include the most useful mix of:
- What user problem was optimized
- What structural changes were made
- Why the new hierarchy or flow is better
- Any important accessibility or responsive considerations
- Follow-up risks or next UX improvements, if relevant
