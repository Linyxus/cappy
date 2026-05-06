#!/usr/bin/env bash
#
# Release the Python-backend artifacts to the gh-pages Maven repo.
#
# Reads `pyVersion` from project/Build.scala, runs sbt with PYBUILD=yes
# against the publishable subset, syncs the staged Maven layout into the
# gh-pages worktree, regenerates apps/scpyc.json, and creates a release
# commit. Does NOT push — the user reviews and pushes manually.
#
# Pre-conditions:
#   - Both the active worktree and ../scala3-py-ghpages must be clean.
#   - ../scala3-py-ghpages must be the gh-pages worktree.
#   - pyVersion in project/Build.scala must be the version to publish.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GHPAGES_WORKTREE="${REPO_ROOT}/../scala3-py-ghpages"
GHPAGES_MAVEN="${GHPAGES_WORKTREE}/maven"
GHPAGES_APPS="${GHPAGES_WORKTREE}/apps"
STAGING="${REPO_ROOT}/target/release-staging"

GROUP_PATH="io/github/linyxus/scalapy"
GHPAGES_BASE_URL="https://linyxus.github.io/scala3-py"

# (sbt task, published module dir under the group path)
PROJECTS_AND_MODULES=(
  "scala3-interfaces|scala3-interfaces-py"
  "scala-library-bootstrapped|scala-stdlib-py"
  "scala3-library-bootstrapped|scala3-library-py_3"
  "tasty-core-bootstrapped|tasty-core-py_3"
  "scala3-compiler-bootstrapped|scala3-compiler-py_3"
  "scala-pylib-py|scala-pylib-py_3"
  "scala-library-py|scala-library-py_3"
)

die() { echo "pyrelease.sh: $*" >&2; exit 1; }

cd "$REPO_ROOT"

# --- Read versions out of Build.scala -------------------------------------
PY_VERSION=$(grep -oE 'val pyVersion\s*=\s*"[^"]+"' project/Build.scala \
             | head -1 | sed -E 's/.*"([^"]+)".*/\1/')
DEV_VERSION=$(grep -oE 'val developedVersion\s*=\s*"[^"]+"' project/Build.scala \
              | head -1 | sed -E 's/.*"([^"]+)".*/\1/')
[[ -n "$PY_VERSION"  ]] || die "could not parse pyVersion from project/Build.scala"
[[ -n "$DEV_VERSION" ]] || die "could not parse developedVersion from project/Build.scala"

FULL_VERSION="${DEV_VERSION}-RC1-PY${PY_VERSION}"
echo "pyrelease.sh: publishing ${FULL_VERSION}"

# --- Pre-flight ------------------------------------------------------------
[[ -d "$GHPAGES_WORKTREE" ]] \
  || die "gh-pages worktree not found at $GHPAGES_WORKTREE — run: git worktree add --orphan -b gh-pages $GHPAGES_WORKTREE"
[[ "$(git -C "$GHPAGES_WORKTREE" rev-parse --abbrev-ref HEAD)" = "gh-pages" ]] \
  || die "$GHPAGES_WORKTREE is not on the gh-pages branch"

# Only block on tracked-file modifications; untracked working files (scratch
# tests, notes/) are the user's normal state and should not gate releases.
# Setting ALLOW_DIRTY=yes bypasses the check (intended for local rehearsal
# only — released artifacts should always come from a clean tree).
if [[ "${ALLOW_DIRTY:-no}" != "yes" ]]; then
  if [[ -n "$(git status --porcelain --untracked-files=no)" ]]; then
    die "main worktree has uncommitted changes — commit or stash first (ALLOW_DIRTY=yes to bypass)"
  fi
  if [[ -n "$(git -C "$GHPAGES_WORKTREE" status --porcelain)" ]]; then
    die "gh-pages worktree is dirty — commit or stash first"
  fi
fi

# Refuse to overwrite an already-published version.
for entry in "${PROJECTS_AND_MODULES[@]}"; do
  module="${entry##*|}"
  target_dir="${GHPAGES_MAVEN}/${GROUP_PATH}/${module}/${FULL_VERSION}"
  if [[ -d "$target_dir" ]]; then
    die "version $FULL_VERSION already published for $module — bump pyVersion in Build.scala"
  fi
done

# --- Build + stage ---------------------------------------------------------
rm -rf "$STAGING"
mkdir -p "$STAGING"

sbt_tasks=()
for entry in "${PROJECTS_AND_MODULES[@]}"; do
  project="${entry%%|*}"
  sbt_tasks+=("${project}/publish")
done

echo "pyrelease.sh: running sbt ${sbt_tasks[*]}"
PYBUILD=yes PY_STAGING_DIR="$STAGING" sbt "${sbt_tasks[@]}"

# --- Validate staging ------------------------------------------------------
[[ -d "${STAGING}/${GROUP_PATH}" ]] \
  || die "staging dir is missing $GROUP_PATH — sbt publish must have failed"

for entry in "${PROJECTS_AND_MODULES[@]}"; do
  module="${entry##*|}"
  staged_dir="${STAGING}/${GROUP_PATH}/${module}/${FULL_VERSION}"
  [[ -d "$staged_dir" ]] || die "missing $staged_dir after publish"
  pom="${staged_dir}/${module}-${FULL_VERSION}.pom"
  jar="${staged_dir}/${module}-${FULL_VERSION}.jar"
  [[ -f "$pom" ]] || die "missing $pom"
  [[ -f "$jar" ]] || die "missing $jar"
done

# --- Sync into gh-pages worktree -------------------------------------------
mkdir -p "${GHPAGES_MAVEN}/${GROUP_PATH}"
# No --delete: prior releases stay in place; new release adds a version dir
# alongside them.
rsync -a "${STAGING}/${GROUP_PATH}/" "${GHPAGES_MAVEN}/${GROUP_PATH}/"

# --- Regenerate apps/scpyc.json --------------------------------------------
cat > "${GHPAGES_APPS}/scpyc.json" <<JSON
{
  "name": "scpyc",
  "main-class": "dotty.tools.dotc.Main",
  "dependencies": [
    "io.github.linyxus.scalapy::scala3-compiler-py:${FULL_VERSION}",
    "io.github.linyxus.scalapy::scala-pylib-py:${FULL_VERSION}",
    "io.github.linyxus.scalapy::scala-library-py:${FULL_VERSION}"
  ],
  "repositories": [
    "central",
    "${GHPAGES_BASE_URL}/maven"
  ],
  "java-options": [
    "-Dscala.usejavacp=true"
  ]
}
JSON

# --- Commit ----------------------------------------------------------------
git -C "$GHPAGES_WORKTREE" add -A
git -C "$GHPAGES_WORKTREE" commit -m "Release ${FULL_VERSION}"

echo
echo "pyrelease.sh: committed ${FULL_VERSION} on the gh-pages branch."
echo "Review the commit, then push:"
echo "    git -C ${GHPAGES_WORKTREE} push -u origin gh-pages"
