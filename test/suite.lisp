;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test :in root-suite))

(def function eval (form)
  (cl:eval (copy-tree form)))

(def function macroexpand (form &optional env)
  (cl:macroexpand (copy-tree form) env))
