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
  (signals error (walk-form/reiterate '(iter named))))

(def test test/basic/clauses-expand ()
  (finishes (macroexpand '(iter (for foo in-list '(1 2 3))
                                (repeat 2)
                                (print foo))))
  (finishes (macroexpand '(iter (for i :in-list '(1 2 3))
                                (collect i)))))

(def test test/basic/collect ()
  (is (equal '(1 2 3)
             (eval '(iter (for i :in-list '(1 2 3))
                          (collect i)))))
  (is (equal '(1 1 1)
             (eval '(iter (repeat 3)
                          (collect 1))))))
