;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/lexenv :in test))

(def test test/lexenv/iteration-variables-show-up-in-lexenv ()
  (eval '(macrolet ((test (&environment env)
                     (is (find-variable-in-lexenv 'x env))
                     (is (find-variable-in-lexenv 'y env))))
          (iter (for x :in-list '(1))
                (for y :in-vector #(1))
                (test)))))
