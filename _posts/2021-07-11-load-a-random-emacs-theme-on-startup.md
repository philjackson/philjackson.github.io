---
layout: post
title:  Load a random emacs theme on startup
date:   Sun 11 Jul 21:16:34 BST 2021
categories: emacs themes doom
---

To load a random emacs theme on startup (I restart daily) you can use
the following snippet. Just add your theme as a new function to
`funs`. If you don't restart often, you could add a timer to change
themes while you're going.

My current favourite themes included.

```elisp
(use-package doom-themes
  :ensure t
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
  :config
  ;; list of themes (just called as functions) which also contain
  ;; theme specific config
  (let* ((funs '((lambda () (load-theme 'doom-outrun-electric))
                 (lambda () (load-theme 'doom-palenight))
                 (lambda ()
                   (load-theme 'doom-challenger-deep t)
                   (set-face-attribute 'markdown-code-face nil
                                       :background "#32333d")
                   (set-face-attribute 'font-lock-comment-face nil
                                       :foreground "#999"))
                 (lambda ()
                   (load-theme 'doom-dracula t)
                   (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                                       :foreground "pink"))))
         (rand (random (length funs))))
    (funcall (nth rand funs)))

  ;; any global theme config goes here
  (set-face-attribute 'show-paren-match nil :foreground "#111" :background "orange")
  (doom-themes-org-config))
```
