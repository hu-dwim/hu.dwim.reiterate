# hu.dwim.reiterate

## What

It's a [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) library for
advanced forms of iteration.

It is inspired by [iterate](https://iterate.common-lisp.dev/), and is in fact
mostly a rewrite of it using a proper code walker
([hu.dwim.walker](https://github.com/hu-dwim/hu.dwim.walker)).

## Why

Iterate has all kind of issues, mostly around code walking. It was causing us
trouble when we used it inside [delimited
continuations](https://github.com/hu-dwim/hu.dwim.delico), or inside the query
compiler in hu.dwim.perec.

Iterate also loses SEXP identity, and because of that Slime's debugger cannot
jump to the forms when the code is inside an `iterate` form. This codebase does
its best to retain the identities.

It was also quite a lot of fun working on it. The ultimate goal was to put
together a library that has an improved API compared to Iterate, and also a
compatibility layer to use it as a drop-in replacement of Iterate.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on [the project's GitHub
page](https://github.com/hu-dwim/hu.dwim.reiterate).

It also has a half-baked page in our [metadata driven
GUI](http://dwim.hu/project/hu.dwim.reiterate), but don't expect much from it.

## Status

Unfortunately, I ran out of steam around 80% completion.

What works is good quality, and the infrastructure is also solid. It has a good
test system, too. IOW, the depth-work is mostly done, but the breadth-work is
not finished.
