#!/usr/bin/env bash
set -euo pipefail

# Backup PostgreSQL database
# Usage: ./scripts/backup.sh [output_file]

OUTPUT_FILE="${1:-backup_$(date +%Y%m%d_%H%M%S).sql}"
PG_CONN="${PG_CONN:-postgresql://postgres:postgres@localhost:5432/idc_catalog}"

echo "Backing up database to $OUTPUT_FILE..."
pg_dump "$PG_CONN" --no-owner --no-privileges --clean --if-exists > "$OUTPUT_FILE"
echo "Backup complete: $OUTPUT_FILE"