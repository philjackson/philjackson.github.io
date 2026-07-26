---
layout: post
title:  memhack, a scanmem-alike for the terminal
date:   Sun 26 Jul 12:20:04 BST 2026
categories: go linux ptrace
---
I've been putting together
[memhack](https://github.com/philjackson/memhack), a memory scanner for Linux
written in Go. You point it at a running process, search its memory for a
value, narrow the matches down as that value changes, then write new values
back.

![Scanning a process and narrowing the matches](/assets/img/memhack-scanner.png)

On Windows this is what [Cheat Engine](https://www.cheatengine.org/) is for,
and on Linux there's already [scanmem](https://github.com/scanmem/scanmem),
with GameConqueror as its GTK front-end. Both are far more capable — Cheat
Engine especially, with its pointer scanning and disassembler. The itch was
having the same thing in a terminal: no GTK, no Wine, and it works over ssh.

The scan expressions are close enough to scanmem's that muscle memory carries
over:

```
memhack> 1337               # every location holding 1337
memhack[3]> dec             # keep the ones that have since decreased
memhack[1]> list
[0] 0x55c8e8e22030 = 1200
memhack[1]> set 0 9999      # write 9999 into match #0
```

`> 100`, `10..20`, `changed`, `inc 5` and friends all do what you'd expect.
That line-based repl is still there behind `-repl`, but the default is a
[Bubble Tea](https://github.com/charmbracelet/bubbletea) TUI, which adds the
things you'd normally want a gui for: values refreshing live, editing a match
in place, and freezing an address so it gets rewritten on an interval and holds
against the program's own writes.
