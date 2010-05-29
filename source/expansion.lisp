;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

;; FIXME expand is called twice?!

(def (macro e) iter (&whole whole &environment lexenv &body body)
  (declare (ignore body))
  (call-expand-from-macro whole lexenv))

(def (macro e) iterate (&whole whole &environment lexenv &body body)
  (declare (ignore body))
  (call-expand-from-macro whole lexenv))

(def layered-method unwalk-form :in reiterate ((loop-form loop-form))
  (call-expand-from-unwalker loop-form))

(def function call-expand-from-unwalker (node)
  (with-active-layers (reiterate)
    (bind ((*loop-form* node)
           (*loop-form-stack* (cons *loop-form* *loop-form-stack*)))
      (expand))))

(def function call-expand-from-macro (whole lexenv)
  (with-active-layers (reiterate)
    (bind ((*loop-form* (walk-form whole :environment (make-walk-environment lexenv)))
           (*loop-form-stack* (cons *loop-form* *loop-form-stack*)))
      (expand))))

(def function expand ()
  (assert (layer-active-p 'reiterate))
  (bind (((:slots name body wrapping-bindings top-label end-label result-form-candidates
                  exit-conditions/before-loop-body exit-conditions/after-loop-body
                  forms/prologue forms/epilogue walk-environment/loop-body) *loop-form*)
         (expansion nil)
         (result-form nil))
    (log.debug "Processing toplevel iterate form ~S; stack is ~A" *loop-form* *loop-form-stack*)
    (setf body (mapcar (lambda (el)
                         (log.debug "Walking body form ~S; stack is ~A" el *loop-form-stack*)
                         (bind ((walked (walk-form el :parent *loop-form* :environment walk-environment/loop-body)))
                           (log.debug "Finished walking body form ~S, will unwalk now" el)
                           (unwalk-form walked)))
                       body))
    (log.debug "Finished walking body of ~A" *loop-form*)
    (cond
      ((slot-boundp *loop-form* 'result-form)
       (setf result-form (result-form-of *loop-form*)))
      ((length= 1 result-form-candidates)
       (setf result-form (cdr (first result-form-candidates))))
      ((not (zerop (length result-form-candidates)))
       (iterate-compile-warning "More than one such clause was used, that can provide a result value. Due to ambiguity the return value will be (VALUES). The form in question is ~A." *loop-form*)))
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
                ,@body
                ,@(generate-exit-jumps exit-conditions/after-loop-body)
                (go ,top-label)
                ,end-label
                ,@forms/epilogue))
      (log.debug "Building result form for ~A" *loop-form*)
      `(block ,name
         (let* (,@wrapping-bindings)
           ,expansion
           ,result-form)))))
