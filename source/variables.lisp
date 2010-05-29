;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def special-variable *loop-form-stack* '())

(def special-variable *loop-form*)

;;;;;;
;;; a very simple hu.dwim.logger simulation so that our emacs coloring kicks in

#+nil
(def macro log.debug (message &rest args)
  `(format *debug-io* ,(string+ message "~%") ,@args))

(def macro log.debug (message &rest args)
  (declare (ignore message args))
  `(values))