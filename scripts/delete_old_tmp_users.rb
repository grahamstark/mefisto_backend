#!/bin/sh
find . -type d -name "tmp_*" -mtime 4 -exec /bin/rm -rf '{}' \;
