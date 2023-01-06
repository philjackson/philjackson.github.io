---
layout: post
title:  Naming a timer in Babashka
date:   Fri  6 Jan 17:04:04 GMT 2023
categories: clojure babashka
---

```clojure
(defmacro named-time
  "Evaluates expr and prints the `nme` and the time it took. Returns the value of
  expr."
  [nme# expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str "Elapsed time (" ~nme# ") "
               (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
               " msecs"))
     ret#))
 ```
 
 Like the `time` function that Babashka/Clojure has in core, but accepts a name
 so that it's easier to identify stuff in a busy console:
 
`(named-time :plus (+ 1 1)) => 2`, prints `"Elapsed time (:plus) 0.022612 msecs"`
