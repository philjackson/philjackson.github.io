---
layout: post
title:  Keeping backups of every edited file
date:   Mon 31 Jan 15:28:13 GMT 2022
categories: emacs
---
I recently ran `rm -rf *` in my home directory, which apart from being
very foolish, reminded me that I have backups of basically every file
I've edited:

```elisp
(setq backup-directory-alist `(("." . "~/.backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
```

Most of those are self-explanatory, but the long-and-short of it is
that I end up with this sort of thing in `~/.backups` (for example):

```
-rw-r--r-- 1 phil phil      593 Nov  7  2020 '!home!phil!.xinitrc.~1~'
-rw-r--r-- 1 phil phil      593 Nov  8  2020 '!home!phil!.xinitrc.~2~'
-rw-r--r-- 1 phil phil      593 Nov  8  2020 '!home!phil!.xinitrc.~3~'
-rw-r--r-- 1 phil phil      593 Nov  8  2020 '!home!phil!.xinitrc.~4~'
-rw-r--r-- 1 phil phil      603 Nov  8  2020 '!home!phil!.xinitrc.~5~'
-rw-r--r-- 1 phil phil      603 Jan  7  2021 '!home!phil!.xinitrc.~6~'
-rw-r--r-- 1 phil phil      785 Oct  6 11:03 '!home!phil!.xinitrc.~7~'
```

The backups rotate and actually don't take up much space at all. Just
20Mb for a few years in my case.
