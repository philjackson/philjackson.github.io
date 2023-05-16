---
layout: post
title:  Hydra for evil-mc
date:   Tue 16 May 14:42:08 BST 2023
categories: emacs hydra doom
---
At least in Doom Emacs the default keybindings for evil-mc are really cumbersome, here's a Hydra that makes things much easier:

```elisp
(defhydra my-mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _z_: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   ^ ^                   _R_: remove all
_n_: skip & next   ^ ^                   _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ("Z" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" #'evil-mc-skip-and-goto-next-match)
  ("N" #'evil-mc-skip-and-goto-prev-match)
  ("J" #'evil-mc-make-cursor-move-next-line)
  ("K" #'evil-mc-make-cursor-move-prev-line)
  ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" #'evil-mc-resume-cursors "quit" :color blue)
  ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))
```

Here's how I map it in Doom:

```elisp
(map!
 (:when (featurep! :editor multiple-cursors)
  :prefix "g"
  :nv "z" #'my-mc-hydra/body))
```
