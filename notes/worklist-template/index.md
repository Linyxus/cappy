# Worklist Index

Entry point for the current worklist, managed by an orchestrating agent.

A worklist is a set of work items. Each item has a short, informative id and is described in `{id}.md` in this directory. Every item must be **actionable**, **verifiable**, and **atomic**. Every item must be registered in the `Work Items` section below.

Each entry in `Work Items` records the item's id, a one-line description, and its *dependencies* (other work items that must complete first).

## Roles

The **main agent** orchestrates; **subagents** execute. Rules:

- Each subagent owns its own git worktree and branch.
- Subagents run only localized, specific tests relevant to their changes: never the full suite. It is too costly.
- When a subagent finishes, it reports back to the main agent. The main agent is responsible for merging the subagent's commits onto the current branch and, when warranted, running the full test suite to check for regressions.
- Subagents must never modify files under `worklist/`. Maintaining the work item files and this index is the main agent's job.
- When a work item is done, the main agent appends a `Results` section to its markdown file summarizing the outcome, then moves the file into the `archived/` subdirectory.
- If completing a work item surfaces follow-ups, the main agent creates new work items for them. The index MUST always be kept up to date.
- The main agent keeps a fixed number of subagents running in parallel, 4 by default, unless the user specifies otherwise.

## Work Items

_To be filled in._
