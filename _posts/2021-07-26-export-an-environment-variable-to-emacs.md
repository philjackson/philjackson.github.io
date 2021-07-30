---
layout: post
title:  Export an environment variable to Emacs
date:   Mon 26 Jul 16:04:50 BST 2021
categories: emacs shell
---

Handy function to export environment variables to Emacs from the command line:

```bash
function export-emacs {
    if [ "$(emacsclient -e t)" != 't' ]; then
        return 1
    fi

    for name in "${@}"; do
        value=$(eval echo \"\${${name}//\\\"/\\\\042}\")
        emacsclient -e "(setenv \"${name}\" \"${value}\")" >/dev/null
    done
}
```

Use it like this:

```bash
export BLAH="Some value"
export-emacs BLAH
```

And in emacs:

```elisp
(getenv "BLAH") ; => "Some value"
```

*update*: A very kind soul [updated the
function](https://github.com/philjackson/philjackson.github.io/issues/1)
to handle quoting properly. Thanks,
[@pcrama](https://github.com/pcrama)!
