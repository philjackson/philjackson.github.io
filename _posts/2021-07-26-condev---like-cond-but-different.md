---
layout: post
title:  condev - like cond but different
date:   Mon 26 Jul 16:12:21 BST 2021
categories: Clojure
---
Hopefully the docstring explains this well enough:

```clojure
(defmacro condev
  "Takes clauses in the same style as `cond` but, if the left side is
  truthy, will execute the right side of the expressions as a function
  which gets the result of evaluation of the left side."
  [& clauses]
  (assert (even? (count clauses)))
  (let [pstep (fn [[test step]] `(when-let [r# ~test] (~step r#)))]
    `(list ~@(map pstep (partition 2 clauses)))))
```

Usage:

```clojure
(condev
 (+ 1 4) #(prn (+ 100 %))
 nil     #(prn "won't happen")
 (+ 1 1) #(prn (+ 200 %))
```
