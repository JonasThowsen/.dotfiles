---
name: agent-orchestrator
description: Orchestrate work across durable pISS agent sessions and registered Git-repository workspaces. Use this skill whenever work should be delegated to one or more agents, parallelized across repositories, handed to a specialist session, reviewed independently, or coordinated through piss_list_workspaces, piss_create_session, piss_send_session, piss_ask_session, piss_collect_responses, or piss_subscribe_responses—even when the user only says to dispatch, fan out, investigate in another workspace, or have another agent review the work.
---

# Agent Orchestrator

Use pISS as the session backend while presenting the workflow to users as **agent orchestration**. pISS provides durable agent sessions attached to registered local workspaces. Each worker has its own context, so give it the facts, constraints, paths, and expected result it needs rather than assuming it can see this conversation.

## Core model

- A **workspace** is a registered canonical directory, normally one Git repository.
- A **session** is a durable worker running inside one workspace.
- A session can receive multiple tasks over time and retains its own conversation context.
- The orchestrator owns sessions it creates and must collect their work before finishing them.
- Repository boundaries matter: a worker in one workspace should not be used as the writer for another repository.

## Tool map

| Tool | Purpose |
|---|---|
| `piss_list_workspaces` | List registered workspaces and their canonical roots. Start here before choosing where work belongs. |
| `piss_create_workspace` | Register an existing approved local directory. Create or clone the directory first with normal filesystem/Git tools. |
| `piss_list_sessions` | List active sessions. Use it before addressing or cleaning up a session. |
| `piss_create_session` | Create a durable worker in a registered workspace. Record the returned session ID. |
| `piss_ask_session` | Send one task and wait synchronously for the worker's response. |
| `piss_send_session` | Dispatch without waiting. Record the returned request ID for later collection or subscription. |
| `piss_collect_responses` | Wait for or collect asynchronous results while the current turn remains active. |
| `piss_subscribe_responses` | Subscribe durably, end the current turn, and wake the orchestrator when any or all requests complete. |
| `piss_finish_session` | Stop and archive a caller-owned session after all its work has completed and been collected. |

The MCP gateway may expose these with a server prefix, but use the underlying `piss_*` operation described above.

## Choose the right workspace

1. Call `piss_list_workspaces`.
2. Match the task to the canonical repository root that owns the files, tests, and commits.
3. Prefer the narrowest correct repository. For a monorepo plus sibling repositories, do not assume the current working directory owns every component.
4. If no workspace matches, verify the intended path with the user or local repository context. Register it with `piss_create_workspace` only when it is an existing approved directory.
5. Use separate sessions for separate repositories. If a change spans repositories, define an explicit task and validation boundary for each worker.

Repository ownership is more important than topical similarity. For example, a deployment tool bug belongs in that tool's repository, while an application deployment configuration belongs in the application's repository.

## Shape a worker task

Keep each assignment bounded and independently verifiable. Include:

- the objective and why it matters;
- the exact repository/workspace and relevant paths;
- known state, commands already tried, and observed errors;
- user constraints and explicit non-goals;
- whether the worker may edit, commit, deploy, or must remain read-only;
- expected checks or tests;
- the response format: findings, changed files, commands run, residual risks, and commit/PR details when applicable.

Workers do not inherit this conversation. Restate critical context in the first prompt. Prefer one coherent deliverable over a broad request such as “finish V1.”

## Dispatch patterns

### Synchronous consultation

Use `piss_ask_session` when the next orchestration decision depends immediately on one worker's answer—for example, a focused investigation, plan review, or final verification.

1. List workspaces and sessions.
2. Create or select the correct session.
3. Ask one bounded question with `piss_ask_session`.
4. Review the evidence before acting.
5. If the caller created the session and no further work remains, finish it after confirming there is nothing pending.

### Asynchronous fan-out

Use `piss_send_session` when tasks are independent and can run in parallel.

1. Create one suitable session per repository or independent workstream.
2. Send each task asynchronously.
3. Save every returned request ID and map it to its session and objective.
4. Continue only work that does not depend on those results.
5. Collect results with `piss_collect_responses`, using `waitFor: "any"` for incremental orchestration or `waitFor: "all"` at a dependency barrier.
6. Review and integrate the results before dispatching follow-ups.

Do not create multiple concurrent writers for the same files or working tree. Parallelize investigation and independent review freely; serialize overlapping mutations through one writer.

### Durable wake-up

Use `piss_subscribe_responses` when work should continue after this turn ends instead of holding the turn open.

- Pass the request IDs returned by `piss_send_session`.
- Choose `waitFor: "any"` to wake on the first completion or `waitFor: "all"` to wake after the whole batch.
- The subscription ends the current turn and wakes the orchestrator with captured responses.
- After waking, process completed work and keep tracking any pending request IDs.

Use `piss_collect_responses` for in-turn waiting; use `piss_subscribe_responses` for durable continuation across turns. Do not use both casually for the same request batch.

## Review and follow-up

Treat worker output as evidence, not authority.

- Check that the response answers the assigned objective.
- Inspect relevant diffs, tests, logs, and commits before accepting mutation work.
- Compare results across workers when they investigated the same risk from different angles.
- Send a focused follow-up to the same session when its retained context is valuable.
- Use a separate read-only session for independent review of consequential changes.
- Keep final synthesis and cross-repository decisions with the orchestrator.

## Session lifecycle and cleanup

1. Track which sessions were created by this orchestrator.
2. Track all request IDs until each response is durably collected.
3. Call `piss_list_sessions` before cleanup. Respect `createdByCaller` and `cleanupRecommended`.
4. Call `piss_finish_session` only when the worker is terminal, has no pending work, and all responses have been collected.
5. Never finish someone else's session merely because it appears idle.

`piss_finish_session` archives history rather than hard-deleting it, but premature cleanup can still lose pending work and is rejected for busy sessions.

## Common mistakes

- **Wrong workspace:** Always inspect canonical roots first; do not route by repository name guesswork.
- **Missing context:** Workers cannot see the parent conversation. Include errors, prior attempts, constraints, and paths.
- **Oversized assignment:** Split broad programs into reviewable deliverables with explicit dependencies.
- **Conflicting writers:** Avoid concurrent edits to the same working tree or files. Assign one writer and independent read-only reviewers.
- **Async without tracking:** Record every request ID immediately. A dispatched task is not complete until its response is collected.
- **Blocking unnecessarily:** Prefer async dispatch for independent work; reserve synchronous asks for immediate decision dependencies.
- **Leaving sessions idle:** Finish caller-owned sessions when all work is collected, rather than accumulating abandoned workers.
- **Finishing too early:** Never finish a busy session or one with uncollected requests.
- **Blind acceptance:** Verify worker claims against diffs, tests, logs, or repository state.
- **Ignoring user boundaries:** Put explicit prohibitions such as “do not deploy,” “read-only,” or “do not add compatibility behavior” directly in the worker prompt.

## Default orchestration checklist

1. Identify repository boundaries and task dependencies.
2. List workspaces and active sessions.
3. Select or create narrowly scoped workers.
4. Dispatch self-contained prompts with permissions and acceptance criteria.
5. Track session IDs and asynchronous request IDs.
6. Collect or subscribe at the appropriate dependency barrier.
7. Review evidence and send focused follow-ups if needed.
8. Synthesize the result for the user, including residual risks.
9. Finish only caller-owned sessions whose work is fully collected.
