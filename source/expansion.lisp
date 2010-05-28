;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def (macro e) iter (&whole whole &environment lexenv &body body)
  (declare (ignore body))
  (expand whole lexenv))

(def (macro e) iterate (&whole whole &environment lexenv &body body)
  (declare (ignore body))
  (expand whole lexenv))

(def function expand (whole &optional lexenv)
  (bind ((*loop-form* (walk-form/reiterate/toplevel whole lexenv))
         (*loop-form-stack* (cons *loop-form* *loop-form-stack*))
         ((:slots name body wrapping-bindings top-label end-label result-form
                  exit-conditions/before-loop-body exit-conditions/after-loop-body
                  forms/prologue forms/loop-body forms/epilogue) *loop-form*)
         (expansion nil))
    (map nil 'process-clause body)
    (flet ((generate-exit-jumps (conditions)
             (loop
               :for condition :in conditions
               :collect `(when ,condition
                           (go ,end-label)))))
      (setf expansion
            `(tagbody
                ,@forms/prologue
                ,top-label
                ,@(generate-exit-jumps exit-conditions/before-loop-body)
                ,@forms/loop-body
                ,@(generate-exit-jumps exit-conditions/after-loop-body)
                (go ,top-label)
                ,end-label
                ,@forms/epilogue))
     `(block ,name
        (let* (,@wrapping-bindings)
          ,expansion
          ,result-form)))))
