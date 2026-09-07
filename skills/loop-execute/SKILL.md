---
name: loop-execute
description: Implement an existing named LOOP Markdown handoff and continue through its remaining work. Use when the user asks to execute or resume a LOOP, preserving its intent across commits and context changes.
---

# Loop Execute

Implement the named LOOP through its requested outcome. A finished checkbox or
commit is a checkpoint, not the end of the assignment. Follow a user-specified
one-item or other stopping boundary when one exists.

Implement the chosen mechanism, including its callers; use examples and tests
to demonstrate that mechanism. An example does not narrow the requested scope.

## Start with intent

Read the LOOP's intent/goal, approach, remaining work, and finish conditions, plus
applicable repository instructions. The user's latest corrections and existing
authorization take precedence over stale plan text. Preserve architectural intent
as well as output behavior; making tests green with a special case is not a
substitute for a requested general mechanism.

Inspect VCS state and the current item's named source, caller, and check. Preserve
unrelated changes. Trust current planning evidence and expand reading only when
the code contradicts it or a concrete question needs answering. Reuse context
across items instead of restarting discovery each time.

Accept older LOOP formats (`WHERE`, `FIX`, `VERIFY`, `DONE WHEN`, etc.) directly.
Missing headings or a different item size do not block understandable work.

## Work, check, continue

1. Take the next unfinished item in dependency order. Relate the change to the
   overall intent before editing; do not write a separate restatement ritual.
2. Implement the chosen approach using local patterns. Resolve ordinary details
   and stale paths from source; correct the LOOP briefly if needed. Keep necessary
   plumbing with its owning behavior and avoid unrelated cleanup.
3. Run the focused check and inspect its actual result. For a bug, use the real
   reproduction or a suitable regression test. Follow explicit test requirements;
   do not impose TDD or add tests that merely mirror the implementation.
4. Review the owned diff for correctness and scope while checking the result.
   Rerun affected checks after a correction. Reuse passing results for unchanged
   code; do not automatically rerun the suite after every review or checkpoint.
5. Check off completed work and record a compact result: what now works, the check
   and observed result, and any remaining limitation. Include the LOOP update with
   the coherent code commit under repo conventions; follow explicit no-commit
   instructions. Stage only owned changes. Report a hash once known; no extra
   documentation commit just to record a hash or narrate the session.
6. Continue immediately to the next eligible item. Keep related work in the same
   context. Do not force compaction after each item; persist the next action and
   evidence when the host needs compaction, then resume from the LOOP and VCS.

Host todos are optional unless the host requires them. Use its actual API, keep
updates small, and treat the LOOP as the source of truth. Do not create a second
work plan or run a repeated add/list/verify protocol. A todo warning about active
unfinished work means continue that work; do not bypass it by calling exit again,
cancelling tasks, or relabeling active items as a future horizon.

## When evidence changes the plan

Fix routine implementation and harness errors within scope. Retry only with a
concrete correction or new evidence; no arbitrary attempt counter. If the chosen
design is contradicted, record `NEEDS PLAN` with the failed assumption, evidence,
and exact design question for the planning model. Continue independent work.
Do not silently choose a replacement architecture or weaken acceptance to get a
checkbox checked. Ask the user only for an actual missing decision or permission;
already approved work stays approved.

Do not invent benchmark thresholds, exact-byte compatibility, new review steps,
or deployment prerequisites. Report measurements as measurements. Preserve
explicit requirements; source checks, emitted artifacts, and observed runtime
behavior support different claims. If a required check cannot run, record that
limitation without claiming it passed or replacing execution with instructions.

## Finish

Run remaining shared checks once, reusing still-current results. Compare the
implemented result with the original intent, including any architectural change.
Stop when the requested work is complete, a user-set boundary is reached, or no
remaining item can proceed without a concrete blocker being resolved. Unfinished
active items are not automatically "later" work. Report the outcome, verification,
and any exact blocker concisely; never stop just to ask whether to continue.
