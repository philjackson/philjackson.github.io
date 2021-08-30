#!/usr/bin/env bash

set -e

# back to the root directory
cd "$(dirname "${BASH_SOURCE[0]}")/.."

cp ~/.emacs _includes/.emacs

git add _includes/.emacs && git commit -m "Updated .emacs"
