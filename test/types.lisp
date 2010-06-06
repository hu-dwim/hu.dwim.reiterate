;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/types :in test))

(def test test/types/for-in-list ()
  ;; problem: the binding of I is introduced with the initial value of NIL which is not of type FIXNUM.
  (with-expected-failures
    (is (equalp '(a x b y)
                (eval '(iter (for (the fixnum i) :in-list '(1 2 3))
                             (collect (1+ i))))))))
