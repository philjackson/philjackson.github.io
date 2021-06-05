---
layout: post
title:  Having Smartparens commands work with Evil-mc
date:   2021-06-05 00:38:51 +0100
categories: elisp evil-mc smartparens
---
[As described
here](https://hungyi.net/posts/how-to-evil-mc-smartparens/), when
using [evil-mc](https://github.com/gabesoft/evil-mc), it's very
frustrating when a [Smartparens](https://github.com/Fuco1/smartparens)
command works on only the first of the multiple cursors. Here's how to
fix that:

```elisp
(dolist (cmd '(sp-up-sexp
               sp-copy-sexp
               sp-down-sexp
               sp-join-sexp
               sp-kill-sexp
               sp-next-sexp
               sp-split-sexp
               sp-wrap-curly
               sp-wrap-round
               sp-raise-sexp
               sp-clone-sexp
               sp-wrap-square
               sp-splice-sexp
               sp-end-of-sexp
               sp-forward-sexp
               sp-backward-sexp
               sp-convolute-sexp
               sp-transpose-sexp
               sp-kill-whole-line
               sp-beginning-of-sexp
               sp-forward-barf-sexp
               sp-forward-slurp-sexp
               sp-backward-barf-sexp
               sp-backward-slurp-sexp
               sp-splice-sexp-killing-forward
               sp-splice-sexp-killing-backward))
    (add-to-list
     'evil-mc-custom-known-commands
     `(,cmd (:default . evil-mc-execute-call))))
```
