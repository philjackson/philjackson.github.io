---
layout: post
title:  Delta for Git and Magit
date:   Thu  9 Sep 14:58:52 BST 2021
categories: magit git emacs delta
---

I was just introduced to a [tool called
Delta](https://github.com/dandavison/delta) which is a pager you can
use with Git to give you Github-esque diffs.

![Magit and Delta](/assets/img/magit-delta.png)

Tiny bit of elisp to configure:

```elisp
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))
```

Delta itself is incredibly configurable and from the command line has
some nice features such as side-by-side diffs.
