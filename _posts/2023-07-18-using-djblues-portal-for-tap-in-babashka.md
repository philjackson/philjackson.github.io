---
layout: post
title:  Using Djblue's portal for tap in Babashka
date:   Tue 18 Jul 21:40:23 BST 2023
categories: babashka clojure
---
I often deal with a repl-crashing amount of data via Babashka and so needed to
find a nice way of browsing data. In clojure I reach for hashp but this time I
thought I'd try out [Chris Badahdah's Portal](https://github.com/djblue/portal)
which describes itself as the following: "A clojure tool to navigate through
your data". [Amazingly, you can even try it out online!](https://djblue.github.io/portal/)

Here's how I set it up:

In bb.edn:

```clojure
{
...
:deps {djblue/portal {:mvn/version "0.42.1"}}
...
}
```

Then in the REPL:

```clojure
(do
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. (fn [] (p/close))))
  (def portal (p/open {:app true}))
  (add-tap #'p/submit))
```

If you have Chrom(ium?) installed, you should get a new window and from this
point, `tap>` will send data there ready to be explored.
