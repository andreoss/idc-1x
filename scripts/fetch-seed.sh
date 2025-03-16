#!/usr/bin/env bash
set -euo pipefail

SEED_DIR="${SEED_DIR:-seed}"
mkdir -p "$SEED_DIR"

echo "Fetching IDC-10..."
curl -fsSL "https://icd.who.int/icd10/2019-01/en/json" -o "$SEED_DIR/idc10.json" 2>/dev/null || {
  echo "WHO API unavailable, trying fallback..."
  curl -fsSL "https://raw.githubusercontent.com/WHO/ICD-10/main/icd10.csv" -o "$SEED_DIR/idc10.csv" 2>/dev/null || {
    echo "Fallback failed, using local seed if present"
    exit 0
  }
}

echo "Fetching IDC-11..."
curl -fsSL "https://icd.who.int/icd11/2023-01/en/json" -o "$SEED_DIR/idc11.json" 2>/dev/null || {
  curl -fsSL "https://raw.githubusercontent.com/WHO/ICD-11/main/icd11.csv" -o "$SEED_DIR/idc11.csv" 2>/dev/null || true
}

echo "Fetching crosswalk..."
curl -fsSL "https://raw.githubusercontent.com/WHO/ICD-11/main/crosswalk.csv" -o "$SEED_DIR/crosswalk.csv" 2>/dev/null || true

echo "Seed data fetched to $SEED_DIR/"