#!/usr/bin/env sh
if command -v Rscript >/dev/null 2>&1; then
  Rscript Install.R
else
  echo "Rscript not found. Please install R." >&2
  exit 1
fi
