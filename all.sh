#!/bin/bash

set -e

STACK_YAML="snapshots/nightly-2017-12-09.yaml" stack setup
STACK_YAML="snapshots/nightly-2017-12-09.yaml" stack build

STACK_YAML="snapshots/lts-9.4.yaml" stack setup
STACK_YAML="snapshots/lts-9.4.yaml" stack build

STACK_YAML="snapshots/lts-6.35.yaml" stack setup
STACK_YAML="snapshots/lts-6.35.yaml" stack build

echo "---------------------------"
echo "All builds were successful!"
echo "---------------------------"
