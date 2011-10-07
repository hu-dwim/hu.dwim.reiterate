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
    (chapter (:title "Origins of the name")
      (paragraph ()
        "The reiterate name was contributed by _3b on #lisp, thanks! It's a pun on iterate, which is a library that highly influenced the API of hu.dwim.reiterate.")))
  (chapter (:title "TODO")
    (paragraph ()
      "the TODO is to be filled up... :)")))

#|

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


|#