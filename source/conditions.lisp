;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2010 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

;; TODO rename to reiterate...

(def (condition* e) iterate-compile-condition ()
  ((loop-form-stack *loop-form-stack*)))

;;;;;;
;;; errors

(def (condition* e) iterate-compile-error (iterate-compile-condition error)
  ())

(def condition* simple-iterate-compile-error (iterate-compile-error simple-error)
  ())

(def function iterate-compile-error (message &rest args)
  (check-type message string)
  (error 'simple-iterate-compile-error :format-control message :format-arguments args))

;;;;;;
;;; warnings

(def (condition* e) iterate-compile-warning (iterate-compile-condition warning)
  ())

(def condition* simple-iterate-compile-warning (iterate-compile-warning simple-warning)
  ())

(def function iterate-compile-warning (message &rest args)
  (check-type message string)
  (warn 'simple-iterate-compile-warning :format-control message :format-arguments args))
