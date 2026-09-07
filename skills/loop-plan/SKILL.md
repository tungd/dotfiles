---
name: loop-plan
description: Create or repair a named LOOP Markdown handoff for another coding model. Use when planning implementation for a faster executor or correcting a LOOP that has drifted from the user's intent.
---

# Loop Plan

Resolve the design here so the executor can implement it without reconstructing
the conversation. Write `LOOP-<short-id>.md`, or the user's chosen path. Keep the
file model-neutral and use the host's built-in plan mode when available.

## Preserve the point

Read the request, applicable instructions, relevant source and tests, and current
VCS state. For a repair, also inspect the failed attempt and the user's corrections.
Separate observed facts from assumptions. Do not implement product code unless
the user also asked for execution.

Lead the LOOP with a short **Intent**:

- The requested outcome and a concrete before/after example.
- The design decision and why it achieves that outcome. Preserve architectural
  requirements as well as visible behavior: relocating special cases does not
  implement a requested general composition mechanism.
- Explicit non-goals and any tempting shortcut that would miss the request.
- What the user has already authorized, including any stated stopping boundary.
  Do not infer additional permissions from the plan.

Translate the user's requirements into observable completion checks. Measurements
are information unless the user explicitly made them acceptance conditions.
Never add a performance threshold, approval checkpoint, compatibility promise,
review requirement, or live-device prerequisite of your own. Preserve actual
repository requirements and distinguish them from recommended checks.

## Make the next work executable

Choose and explain the implementation approach; do not hand off a menu of designs.
Locate the real caller-to-result path and the existing pattern to follow. For a
cross-layer change, start with the smallest useful path through those layers,
then extend coverage. Preparatory work is fine when needed; say what behavior it
unlocks so it cannot become the new goal.

For a non-obvious transformation, show a short algorithm or worked example:
input, intermediate representation, result, and what must remain true. Translate
terms such as "generic", "algebraic", or "parametric" into concrete operations
and ownership so the executor can implement the decision rather than infer it.

Use as many items as the work needs, with no checkbox or file-count quota. Each
item should express one understandable change. Split at a meaningful behavior or
dependency boundary, not just to make another commit. Expand implementation
details only where the source and design support them. If an investigation is
still needed, state its question and expected finding instead of disguising it
as a ready coding task; do not invent a user decision gate.

Use this compact shape, adapting headings to an existing LOOP:

```markdown
# LOOP: <outcome>
## Intent
<outcome, example, chosen approach, boundaries, existing authorization>

## Context
<repo and relevant paths/symbols with roles; governing docs; verified facts>

## Work
- [ ] 1. <behavior or necessary enabling change>
  Change: <file:symbol, concrete change, why it advances the intent>
  Check: <cwd, command or observation, expected result tied to the request>

## Finish
<overall requested result and any remaining required checks>
```

Add dependencies, a code sketch, an example to follow, or a specific pitfall only
where they remove a real executor decision. Name current paths and symbols; mark
new ones as planned. Link existing governing docs and write/update an ADR or
design note when a durable decision needs it. No empty fields, N/A inventory,
duplicated file maps, or documentation by category.

Distinguish checks of source, generated output, and actual runtime behavior.
Choose checks that exercise the requested change; passing old tests alone may
say nothing about it. Reuse focused checks and put shared suite commands once
under Finish instead of repeating them in every item. Do not require a new test
suite or review-only item for routine work.

## Hand off or repair

Read the finished plan as an executor who has no chat history: can it locate the
code, understand why this change is right, and recognize the requested result?
Resolve gaps from source or docs before handing off. Keep existing IDs and valid
completion evidence when repairing; update stale instructions and the next action
in place rather than appending competing plans or a long session diary.

The LOOP is the durable work queue; host todos are optional progress mirrors.
All unfinished work in the active LOOP remains execution scope unless the user
explicitly deferred it. A commit, checkpoint, or context compaction does not end
that scope. Conclude with the file path and next action for `/loop-execute`.
