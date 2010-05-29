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
  (assert (reiterate-toplevel-macro-name? (first whole)))
  (with-active-layers (reiterate)
    (bind ((*loop-form* (walk-form (if (eq 'iterate (first whole))
                                       whole
                                       (cons 'iterate (rest whole)))
                                   :environment (make-walk-environment lexenv)))
           (*loop-form-stack* (cons *loop-form* *loop-form-stack*))
           ((:slots name body wrapping-bindings top-label end-label result-form-candidates
                    exit-conditions/before-loop-body exit-conditions/after-loop-body
                    forms/prologue forms/epilogue walk-environment/loop-body) *loop-form*)
           (expansion nil)
           (result-form nil))
      (log.debug "Toplevel iterate form ~S, ~A" whole *loop-form*)
      (setf body (mapcar [walk-form !1 :parent *loop-form* :environment walk-environment/loop-body] body))
      (cond
        ((slot-boundp *loop-form* 'result-form)
         (setf result-form (result-form-of *loop-form*)))
        ((length= 1 result-form-candidates)
         (setf result-form (cdr (first result-form-candidates))))
        ((not (zerop (length result-form-candidates)))
         (iterate-compile-warning "More than one such clause was used, that can provide a result value. Due to ambiguity the return value will be (VALUES). The form in question is ~S." whole)))
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
                  ,@(mapcar 'unwalk-form body)
                  ,@(generate-exit-jumps exit-conditions/after-loop-body)
                  (go ,top-label)
                  ,end-label
                  ,@forms/epilogue))
        `(block ,name
           (let* (,@wrapping-bindings)
             ,expansion
             ,result-form))))))
