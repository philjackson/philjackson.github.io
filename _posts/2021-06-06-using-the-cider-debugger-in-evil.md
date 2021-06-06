---
layout: post
title:  Using the CIDER debugger in Evil
date:   Sun  6 Jun 17:00:40 BST 2021
categories: emacs evil cider debugging
---

When using [evil](https://github.com/emacs-evil/evil) and
[CIDER](https://github.com/clojure-emacs/cider) together, you might
find that rebinding the keys required to control the debugger is a
pain. Luckily, you don't actually have to, you can just enter insert
mode instead:

```elisp
(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode    ;; Checks if you're entering the debugger
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state

(add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)
```

Borrowed from [here.](https://emacs.stackexchange.com/questions/20804)
