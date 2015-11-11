;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.documentation)

(def project :hu.dwim.reiterate :path (system-pathname :hu.dwim.reiterate)
  :description "General iteration/looping macro")

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (chapter (:title "Motivation for the project")
      (paragraph ()
        "Iterate has several shortcomings, e.g. its code walker gets tripped up in corner cases, it does not properly update the environment with the variables it introduces, which breaks non-trivial macros inside the body of iterate."))
    (chapter (:title "Origins of the name")
      (paragraph ()
        "The reiterate name was contributed by _3b on #lisp, thanks! It's a pun on iterate, which is a library that highly influenced the API of hu.dwim.reiterate.")))
  (chapter (:title "TODO")
    (paragraph ()
      "the TODO is to be filled up... :)")))

#|

implement :in-sequence

flip215: attila_lendvai1: feature request for reiterate: I want to _build_ a vector of size N, and set its elements.
(14:38:12) flip215: similar to (make-array N :initial-contents ...)

attila: variable allocation and assignment should be flexible enough that most features should work with multiple values and destructuring bind. i'm not sure that that's the case already, but it's noted.

Philipp Marek <philipp@marek.priv.at>
> Eg. an easier way for multiple previous - eg. this works:
>    (iterate:iter
>      (for e in '(1 2 3 4 5 6 7 8 9))
>      (for d previous e)
>      (for c previous d)
>      (for b previous c)
>      (for a previous b)
>      (print (list a b c d e)))
>  but this doesn't:
>    (iterate:iter
>      (for e in '(1 2 3 4 5 6 7 8 9))
>      (for (d c b a) previous (list e d c b))
>      (print (list a b c d e)))


strided iteration: it just means iterating across elements x_a, x_{a+s}, x_{a+2s} ...

(iter (collecting (...) into (values a b c)))

(progn
        (load-system :hu.dwim.logger)
        (develop-system :hu.dwim.reiterate)
        (eval (read-from-string "(setf (log-level/runtime 'log) +info+)")))


maybe a symbol-macrolet to make sequence iteration variables setf-able?

|#
