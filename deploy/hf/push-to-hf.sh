#!/usr/bin/env bash
# ------------------------------------------------------------------
# Deploy the TRISK Shiny demo to a Hugging Face Docker Space.
#
# Assembles a self-contained Space repo (Dockerfile + app + scenario
# data + Space card), then creates the Space and pushes it. HF builds
# the image and serves it at https://<user>-<space>.hf.space
#
# Prerequisites:
#   - git, git-lfs, curl
#   - A Hugging Face account + a write token: https://huggingface.co/settings/tokens
#
# Usage:
#   export HF_TOKEN=hf_xxx          # write-scoped token
#   export HF_USERNAME=your-handle
#   export SPACE_NAME=trisk-demo    # optional, defaults to trisk-demo
#   bash deploy/hf/push-to-hf.sh
# ------------------------------------------------------------------
set -euo pipefail

# --- Resolve paths ------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# --- Config -------------------------------------------------------
SPACE_NAME="${SPACE_NAME:-trisk-demo}"
: "${HF_USERNAME:?Set HF_USERNAME (your Hugging Face handle)}"
: "${HF_TOKEN:?Set HF_TOKEN (write token from https://huggingface.co/settings/tokens)}"

SPACE_ID="${HF_USERNAME}/${SPACE_NAME}"
SCENARIOS="${REPO_ROOT}/data/scenarios/scenarios.csv"

echo "==> Target Space: ${SPACE_ID}"

# --- Preflight ----------------------------------------------------
command -v git      >/dev/null || { echo "git not found";      exit 1; }
command -v git-lfs  >/dev/null || { echo "git-lfs not found — install: brew install git-lfs"; exit 1; }
command -v curl     >/dev/null || { echo "curl not found";     exit 1; }

if [ ! -f "${SCENARIOS}" ]; then
  echo "Scenario data missing. Fetching it first..."
  bash "${REPO_ROOT}/scripts/download_scenarios.sh"
fi

# --- Create the Space (idempotent) --------------------------------
echo "==> Ensuring Space exists on Hugging Face..."
CREATE_RESP="$(curl -fsS -X POST "https://huggingface.co/api/repos/create" \
  -H "Authorization: Bearer ${HF_TOKEN}" \
  -H "Content-Type: application/json" \
  -d "{\"type\":\"space\",\"name\":\"${SPACE_NAME}\",\"sdk\":\"docker\"}" \
  2>/dev/null || true)"
echo "    ${CREATE_RESP:-(already exists or no response — continuing)}"

# --- Assemble the Space tree --------------------------------------
BUILD_DIR="${SCRIPT_DIR}/.space-build"
rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}/app" "${BUILD_DIR}/scripts" "${BUILD_DIR}/data/scenarios"

echo "==> Assembling Space repo in ${BUILD_DIR}"
cp "${SCRIPT_DIR}/Dockerfile.hf"      "${BUILD_DIR}/Dockerfile"
cp "${SCRIPT_DIR}/space-README.md"    "${BUILD_DIR}/README.md"
cp "${SCRIPT_DIR}/.gitattributes"     "${BUILD_DIR}/.gitattributes"
cp -R "${REPO_ROOT}/app/."            "${BUILD_DIR}/app/"
cp "${REPO_ROOT}/scripts/run-shiny.sh" "${BUILD_DIR}/scripts/run-shiny.sh"
cp "${SCENARIOS}"                     "${BUILD_DIR}/data/scenarios/scenarios.csv"

# Drop local cruft that shouldn't ship to the Space
find "${BUILD_DIR}/app" -name '.DS_Store' -delete 2>/dev/null || true
find "${BUILD_DIR}/app" -name '.Rhistory' -delete 2>/dev/null || true

# --- Init, commit, push -------------------------------------------
cd "${BUILD_DIR}"
git init -q
git lfs install --local >/dev/null
git lfs track "data/scenarios/scenarios.csv" >/dev/null
git add -A
git -c user.email="deploy@local" -c user.name="trisk-deploy" commit -qm "Deploy TRISK demo to Hugging Face Spaces"
git branch -M main

REMOTE="https://${HF_USERNAME}:${HF_TOKEN}@huggingface.co/spaces/${SPACE_ID}"
git remote add origin "${REMOTE}" 2>/dev/null || git remote set-url origin "${REMOTE}"

echo "==> Pushing to Hugging Face (this uploads the 59MB scenario file via LFS)..."
git push -f origin main

echo ""
echo "✓ Pushed. HF is now building the image."
echo "  Build logs:  https://huggingface.co/spaces/${SPACE_ID}?logs=build"
echo "  Live app:    https://${HF_USERNAME}-${SPACE_NAME}.hf.space"
echo "  (First build takes ~15–30 min while R packages compile.)"
