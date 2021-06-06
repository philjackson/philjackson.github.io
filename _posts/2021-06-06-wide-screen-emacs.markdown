---
layout: post
title:  Widescreen emacs and vertical splits
date:   2021-06-06 13:14:51 +0100
categories: emacs widescreen
---
On a widescreen monitor, especially the really wide ones, it's nice to
have only vertical splits. This snippet will give bias to vertical splits:

```elisp
(defun my-split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers
vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)
```
