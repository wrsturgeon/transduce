#!/bin/sh

set -eux

if [ $# -eq 0 ]
then
  echo 'Please provide a commit message'
  exit 1
fi

git add -A
git commit -m "$@"
git push
