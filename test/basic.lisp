;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/basic :in test))

(def test test/basic/naming ()
  (is (eq (name-of (walk-form/reiterate '(iter alma)))
          'alma))
  (is (eq (name-of (walk-form/reiterate '(iter named alma)))
          'alma))
  (finishes (macroexpand '(iter (for foo in-list '(1 2 3))
                                (repeat 2)
                                (print foo))))
  (signals error (walk-form/reiterate '(iter named))))

(def test test/basic/clauses ()
  (finishes (macroexpand '(iter (for foo in-list '(1 2 3))
                                (repeat 2)
                                (print foo)))))

#+nil
(def test test/basic/1 ()
  (is (equal (iter (repeat 3)
                   (collect 1))
             '(1 1 1))))
