#!/bin/sh

SQL_FILE="$1"
DB="$2"

if [ -f "$DB" ]; then
  echo "$DB already exists, attempting to update."
fi

sqlite3 "$DB" < "$SQL_FILE"
