#!/bin/bash
from=aleph.gutenberg.org::gutenberg
to=/mnt/vol/pg-mirror-new/mirror
# Command for only rsyncing certain types of files. 
# Adapted from https://stackoverflow.com/a/32527277/584121
rsync -zarv --prune-empty-dirs --exclude "old/" --exclude "cache/" --include "*/"  --include="*.html" --exclude="*" "$from" "$to"
rsync -zarv --prune-empty-dirs --exclude "old/" --exclude "cache/" --include "*/"  --include="*.txt" --exclude="*" "$from" "$to"
