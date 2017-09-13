#!/bin/bash

set -e

STACK_YAML="lts/lts-9.4.yaml" stack setup
STACK_YAML="lts/lts-9.4.yaml" stack build

STACK_YAML="lts/lts-6.35.yaml" stack setup
STACK_YAML="lts/lts-6.35.yaml" stack build

STACK_YAML="lts/lts-2.22.yaml" stack setup
STACK_YAML="lts/lts-2.22.yaml" stack build

echo "---------------------------"
echo "All builds were successful!"
echo "---------------------------"
