#!/usr/bin/env bash
# Download scenario data for TRISK Docker build.
# Run this once before your first `docker compose build`.
#
# Bank users: if your environment cannot reach public internet, obtain
# scenarios.csv through your internal data governance process and place it
# at data/scenarios/scenarios.csv. The Docker build will verify the SHA256
# checksum regardless of how the file was obtained.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

DEST="${REPO_ROOT}/data/scenarios/scenarios.csv"
URL="https://storage.googleapis.com/crispy-public-data/trisk_inputs/scenarios.csv"
SHA256="b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56"

if [ -f "$DEST" ]; then
  echo "Scenarios file already exists: $DEST"
  echo "Verifying checksum..."
  echo "${SHA256}  ${DEST}" | sha256sum -c - && echo "✓ Checksum OK" && exit 0
  echo "✗ Checksum mismatch — re-downloading..."
fi

mkdir -p "$(dirname "$DEST")"
echo "Downloading scenarios.csv from public GCS..."
curl -fsSL -o "$DEST" "$URL"
echo "${SHA256}  ${DEST}" | sha256sum -c -
echo "✓ Scenarios downloaded and verified: $DEST"
