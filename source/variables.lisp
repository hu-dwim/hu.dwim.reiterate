;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def constant +toplevel-macro-aliases+ '(iter iterate))

(def macro for-each-iterator-alias (alias-variable-name &body body)
  `(macrolet ((body-emitter (,alias-variable-name)
                ,@body))
     ,@(loop
         :for alias :in +toplevel-macro-aliases+
         :collect `(body-emitter ,alias))))

(def special-variable *loop-form-stack* '())

(def special-variable *loop-form*)

(def special-variable *clause*)

;;;;;;
;;; a very simple hu.dwim.logger simulation so that our emacs coloring kicks in

#+nil
(def macro log.debug (message &rest args)
  `(format *debug-io* ,(string+ message "~%") ,@args))

(def macro log.debug (message &rest args)
  (declare (ignore message args))
  `(values))
