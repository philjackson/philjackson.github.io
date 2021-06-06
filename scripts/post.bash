#!/usr/bin/env bash

set -e

# back to the root directory
cd "$(dirname "${BASH_SOURCE[0]}")/.."

title="${1}"
if [[ -z "${title}" ]]; then
    echo "Please provide a title for the post." >&2
    exit 1
fi

filename="_posts/$(date +%Y-%m-%d)-$(echo $title | sed -e 's/ \+/-/g' -e 's/\(.*\)/\L\1/').md"

echo -e "---
layout: post
title:  ${title}
date:   $(date)
categories: 
---
" > "${filename}"

echo "${filename}"
