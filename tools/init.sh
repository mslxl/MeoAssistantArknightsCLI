#!/bin/bash

set -e

echo "Cloning..."
bash ./clone.sh
echo "Building..."
bash ./build.sh

ln -s "$(realpath result)" "$(realpath ../lib)"