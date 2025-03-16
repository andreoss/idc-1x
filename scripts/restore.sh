#!/usr/bin/env bash
set -euo pipefail

# Restore PostgreSQL database from backup
# Usage: ./scripts/restore.sh backup_file.sql

BACKUP_FILE="${1:-}"

if [[ -z "$BACKUP_FILE" ]]; then
  echo "Usage: $0 <backup_file.sql>"
  exit 1
fi

if [[ ! -f "$BACKUP_FILE" ]]; then
  echo "Backup file not found: $BACKUP_FILE"
  exit 1
fi

PG_CONN="${PG_CONN:-postgresql://postgres:postgres@localhost:5432/idc_catalog}"

echo "Restoring database from $BACKUP_FILE..."
psql "$PG_CONN" -f "$BACKUP_FILE"
echo "Restore complete"