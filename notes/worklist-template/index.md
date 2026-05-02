# Worklist Index

This is the entry file for the current worklist, which is to be managed by an agent.

The worklist consists of work items. Each work item has an informative id and is described in `{id}.md` in the current directory. Each task must be actionable, verifiable, atomic. All task items must be recorded in the index (in the `Work Items` section in the following).

The `Work Item` section records a list of work items: their id, their simple description, and their *dependencies*. Each work item can declare other work items as pre-requistes.

The orchestrating agent (the main agent) assigns work items to subagents. Rules:
- Each subagent should own their own git worktree. They work on their own branch.
- Subagents should not run costly full-suite tests. They run tests that are localized, specific to their changes.
- After a subagent finishes its work, it reports back to the main agent. The main agent is responsible for merging their changes / commits onto the current branch. And when necessary, run full tests to check regressions.
- Subagents should never modify files inside the `worklist/` directory. It is the main agent's job to maintain the work item files and this index.md file.
- When a workitem is done, the main agent summarises the status of that work item and append as a "Results" section to the markdown file. The markdown file should also be move to a subdirectory called `archived/`.
- When a workitem is done, it could need follow-ups. In this case, the main agent should create new work items. The index MUST always be kept up-to-date.
- The main agent keeps a fixed number of parallel sub-agents running. By default, it is 4. The user may specify it explicitly.

## Work Items

The list of work item is to be filled here.
