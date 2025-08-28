# hu.dwim.reiterate

## What

It's a [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) library for
advanced forms of iteration.

It is inspired by [iterate](https://iterate.common-lisp.dev/), and is in fact
mostly a rewrite of it using a [proper code
walker](https://github.com/hu-dwim/hu.dwim.walker).


## Why

Iterate has various issues around code walking. It was causing
us trouble when we used it inside [delimited
continuations](https://github.com/hu-dwim/hu.dwim.delico), or inside
the Lisp -> SQL query compiler in
[hu.dwim.perec](https://github.com/hu-dwim/hu.dwim.perec)
(e.g. iterate is not extending the lexical environment with the
variables it is introducing).

Iterate doesn't retain the identity of the cons cells, and due to that
jumping to the source location inside an `iterate` form doesn't work
in Slime. This codebase retains the identities whenever possible.

It was also quite a lot of fun working on it. The ultimate goal was to put
together a library that has an improved API compared to Iterate, and also a
compatibility layer so that it can be used as a drop-in replacement for Iterate.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).


## Where

The primary communication channel is the facilities on [the project's GitHub
page](https://github.com/hu-dwim/hu.dwim.reiterate).


## Status

Unfortunately, it's at around 80% completion, and I haven't worked on
it for years. 

You're welcome to take over development if you feel inspired!

Whatever is finished is of good quality, and the infrastructure is
also solid. It has good unit tests, too. In short, all the depth-work
is probably done, but there's more breadth-work to be done.
