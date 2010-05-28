;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/nesting :in test))

(def test test/nesting/1 ()
  (with-expected-failures
    (is (equal '(a b c)
               (eval '(macrolet ((wrapper (&body body)
                                  `(iter named inner
                                         (repeat 2)
                                         ,@body)))
                       (iter named outer
                             (for i :in-list '(a b c))
                             (wrapper
                              ;; this collect should collect into OUTER
                              (collect i)))))))))
