---
layout: post
title:  Start evil substitution on selection
date:   Sun 11 Jul 11:23:27 BST 2021
categories: evil emacs
---

Here's a little evil operator that will fill in the first part of an
evil/ex substitution with the region / visual / object. This allows
you to replace objects quickly without having to type out the ex
command in full.

If I've missed something like this that's built-in, let me know.

```elisp
(evil-define-operator start-ex-sub-on-region (beg end)
  (let ((region (buffer-substring beg end)))
    (evil-ex (concat "%s/" (replace-regexp-in-string "\/" "\\\\/"
                                                     (regexp-quote region))
                     "/"))))
```

I bind this globally to `",s"` with:

```elisp
(evil-global-set-key 'normal (kbd ",s") 'start-ex-sub-on-region)
```
