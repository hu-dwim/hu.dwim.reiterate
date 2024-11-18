# hu.dwim.reiterate

## What

It's a [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) library for
advanced forms of iteration.

It is inspired by [iterate](https://iterate.common-lisp.dev/), and is in fact
mostly a rewrite of it using a [proper code
walker](https://github.com/hu-dwim/hu.dwim.walker).


## Why

Iterate has all kinds of issues, mostly around code walking. It was causing us
trouble when we used it inside [delimited
continuations](https://github.com/hu-dwim/hu.dwim.delico), or inside the query
compiler in hu.dwim.perec (e.g. it is not extending the lexical environment with
the variables it is introducing).

Iterate also loses [SEXP](https://en.wikipedia.org/wiki/S-expression) identity,
and because of that Slime's debugger cannot jump to the forms when the code is
inside an `iterate` form. This codebase does its best to retain the identities.

It was also quite a lot of fun working on it. The ultimate goal was to put
together a library that has an improved API compared to Iterate, and also a
compatibility layer so that it can be used as a drop-in replacement for Iterate.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).


## Where

The primary communication channel is the facilities on [the project's GitHub
page](https://github.com/hu-dwim/hu.dwim.reiterate).


## Status

Unfortunately, I ran out of steam at around 80% completion, and the project has
been abandoned for years.

You're welcome to take over development if you feel inspired!

What works is of good quality, and the infrastructure is also solid. It has a
good test system, too. IOW, the depth-work is mostly done, but there's more
breadth-work to be done.
