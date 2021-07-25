---
layout: post
title:  Using yadm via magit
date:   Sun 25 Jul 18:22:13 BST 2021
categories: yadm emacs magit
---

[Yadm's](https://yadm.io/) an amazing dotfile manager that's basically
a thin wrapper around git and a bare repo. With it being a bare repo,
you'll not be able to manage it directly with Magit so here's a really
[smart tip I found](https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/)
that will let you access it via Tramp:

```elisp
(use-package tramp
  :config
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

(defun yadm ()
  (interactive)
  (magit-status "/yadm::"))
```
