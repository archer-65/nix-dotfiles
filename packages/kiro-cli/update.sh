#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl jq gnused gnugrep

set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PACKAGE_NIX="${SCRIPT_DIR}/package.nix"
SOURCES_JSON="${SCRIPT_DIR}/sources.json"

manifest_url="https://desktop-release.q.us-east-1.amazonaws.com/latest/manifest.json"
manifest="$(curl -sL "$manifest_url")"

version="$(jq -r '.version' <<< "$manifest")"

#
# --- Extract artifacts ---
#

linux_x86_64="$(jq -r '
  .packages[]
  | select(.os == "linux")
  | select(.architecture == "x86_64")
  | select(.variant == "headless")
  | select(.fileType == "tarXz")
  | select(.targetTriple == "x86_64-unknown-linux-gnu")
  ' <<< "$manifest")"

linux_aarch64="$(jq -r '
  .packages[]
  | select(.os == "linux")
  | select(.architecture == "aarch64")
  | select(.variant == "headless")
  | select(.fileType == "tarXz")
  | select(.targetTriple == "aarch64-unknown-linux-gnu")
  ' <<< "$manifest")"

darwin_universal="$(jq -r '
  .packages[]
  | select(.os == "macos")
  | select(.architecture == "universal")
  | select(.fileType == "dmg")
  ' <<< "$manifest")"

linux_x86_64_url="$(jq -r '.download' <<< "$linux_x86_64")"
linux_x86_64_sha="$(jq -r '.sha256' <<< "$linux_x86_64")"

linux_aarch64_url="$(jq -r '.download' <<< "$linux_aarch64")"
linux_aarch64_sha="$(jq -r '.sha256' <<< "$linux_aarch64")"

darwin_unescaped_url="$(jq -r '.download' <<< "$darwin_universal")"
darwin_sha="$(jq -r '.sha256'   <<< "$darwin_universal")"

# Escape spaces ONLY for the DMG
darwin_url="${darwin_unescaped_url// /%20}"

#
# --- NEW: version check & package.nix update ---
#

if [[ ! -f "$PACKAGE_NIX" ]]; then
    echo "Error: package.nix not found at $PACKAGE_NIX" >&2
    exit 1
fi

current_version=$(grep -E '^  version = "' "$PACKAGE_NIX" | cut -d'"' -f2 || true)

if [[ -z "$current_version" ]]; then
    echo "Error: could not extract version from package.nix" >&2
    exit 1
fi

if [[ "$version" == "$current_version" ]]; then
    echo "No update needed. Current version is already the latest: $current_version"
    exit 0
fi

echo "Updating package.nix: $current_version → $version"

# update version field
sed -i "s/version = \".*\"/version = \"$version\"/" "$PACKAGE_NIX"

#
# --- Write new sources.json ---
#

cat > "$SOURCES_JSON" <<EOF
{
  "x86_64-linux": {
    "url": "https://desktop-release.q.us-east-1.amazonaws.com/$linux_x86_64_url",
    "sha256": "$linux_x86_64_sha"
  },
  "aarch64-linux": {
    "url": "https://desktop-release.q.us-east-1.amazonaws.com/$linux_aarch64_url",
    "sha256": "$linux_aarch64_sha"
  },
  "x86_64-darwin": {
    "url": "https://desktop-release.q.us-east-1.amazonaws.com/$darwin_url",
    "sha256": "$darwin_sha"
  },
  "aarch64-darwin": {
    "url": "https://desktop-release.q.us-east-1.amazonaws.com/$darwin_url",
    "sha256": "$darwin_sha"
  }
}
EOF

echo "Updated to version $version"
