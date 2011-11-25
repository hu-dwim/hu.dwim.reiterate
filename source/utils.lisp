;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def constant +initial-value/cons+ (cons nil nil))

(def function initial-value-for-type (type)
  (flet ((match (value)
           (return-from initial-value-for-type (values value #t))))
    (case type
      (list (match nil))
      (cons (match `(quote ,+initial-value/cons+)))
      ((#.+top-type+) (match nil))
      (otherwise
       (cond
         ((subtypep type 'number) (match 0))
         ((subtypep type 'string) (match ""))
         ((subtypep type 'symbol) (match nil)))))
    nil))
