#!/bin/sh

set -eux

cargo fmt -- --check
cargo build --no-default-features --verbose
cargo build --all-features --verbose
set +u
if [ -z "${GITHUB_ACTIONS}" ]
then
  set -u
  export PROPTEST_CASES=10000
  if cargo test --verbose --no-default-features && cargo test --verbose --all-features
  then
    : # all good
  else
    cat proptest-regressions/base.txt
    exit 1
  fi
else
  set -u
  export PROPTEST_CASES=1000000
  if cargo test --verbose --no-default-features && cargo test --verbose --all-features
  then
    : # all good
  else
    cat proptest-regressions/base.txt
    exit 1
  fi
fi
cargo clippy --all-targets --verbose --no-default-features
cargo clippy --all-targets --verbose --all-features
