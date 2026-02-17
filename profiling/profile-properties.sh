#!/usr/bin/env bash
set -euo pipefail

PROFILE_LOG=profiling/results/properties-for-spec.csv
mkdir -p profiling/results

echo "running bench-properties at $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
output=$(cabal run bench-properties 2>&1)
echo "$output"

generated=$(printf "%s" "$output" | awk -F': ' '/generated properties/ {print $2}')
elapsed=$(printf "%s" "$output" | awk -F': ' '/generation time/ {print $2}')
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

if [[ -z $generated || -z $elapsed ]]; then
  echo "failed to parse bench output" >&2
  exit 1
fi

if [[ ! -f $PROFILE_LOG ]]; then
  printf "timestamp,properties,generation_time\n" >"$PROFILE_LOG"
fi

printf "%s,%s,%s\n" "$timestamp" "$generated" "$elapsed" >>"$PROFILE_LOG"
echo "appended profile data to $PROFILE_LOG"
