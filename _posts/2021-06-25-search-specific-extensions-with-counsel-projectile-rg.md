---
layout: post
title:  Search specific extensions with counsel-projectile-rg
date:   Fri Jun 25 10:31:02 PM BST 2021
categories: emacs search rg
---
If you're in a monorepo searching through stuff can be a bit chaotic. Here's a way of narrowing down to specific file extensions with `counsel-projectile-rg`:

```elisp
(defun search-specific-glob ()
    (interactive)
    (let ((glob (ivy-completing-read "Glob?: " '("*.cljs"
                                                 "*.clj"
                                                 "*.md"
                                                 "*.styl"
                                                 "*.css"))))
      (counsel-projectile-rg (concat "--glob " glob))))
```

First it'll list the extensions with Ivy, then lets you input a search term as normal.
