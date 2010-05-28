;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def test test/basic/1 ()
  (is (equal (iter (repeat 3)
                   (collect 1))
             '(1 1 1))))
