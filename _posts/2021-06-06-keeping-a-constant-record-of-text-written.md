---
layout: post
title:  Keeping a constant record of text written
date:   Sun  6 Jun 14:54:16 BST 2021
categories: text shell writing
---

If ever I'm writing a chunk of text that will change a lot over time,
I like to use this script to make sure I have a constant record of
what has changed over time. Combined with your equivelent of
[git-timemachine](https://github.com/emacsmirror/git-timemachine),
it's really handy for looking back at progression or finding where you
might have gone wrong. 🙂

Just run `git init` if you're not already in a repo, then run this
script with the filename(s) you're interested in keeping the history
for. Every two minutes, a new commit will be made with your work.

Just delete the `.git` folder when you're done and you'll be left with
just your work.

```bash
#!/usr/bin/env bash

[ ${#} -lt 1 ] && {
  echo "Need a list of files." >&2
  exit 2
}

words=$(cat "${@}" | wc -w)

while /bin/true; do
  git add ${@} && git commit -m "$(date) ($words words)"
  sleep 120
done
```
