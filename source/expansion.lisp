;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(for-each-iterator-alias alias
  `(def (macro e) ,alias (&whole whole &environment lexenv &body body)
     (declare (ignore body))
     (call-expand-from-macro whole lexenv)))

(def layered-method unwalk-form :in reiterate ((loop-form loop-form))
  (call-expand-from-unwalker loop-form))

(def function call-expand-from-unwalker (node)
  (with-active-layers (reiterate)
    (bind ((*loop-form* node)
           (*loop-form-stack* (cons *loop-form* *loop-form-stack*)))
      (expand))))

(def function call-expand-from-macro (whole lexenv)
  ;; We must to make sure FORM does not have reused CONS cells because we use the CONS identities to
  ;; identify which iter macro they are coming from in case of nesting.
  ;; It can happen in compiled code (e.g. on SBCL this is and endless loop when used inside a defun
  ;; because the two REPEATs have the same identity: (iter (repeat 2) (iter (repeat 2)))
  (setf whole (copy-tree whole))
  (with-active-layers (reiterate)
    (bind ((*loop-form* (walk-form whole :environment (make-walk-environment lexenv)))
           (*loop-form-stack* (cons *loop-form* *loop-form-stack*)))
      (expand))))

(def function variable-bindings/extract-primitive-bindings (variable-alist)
  (mapcar (lambda (entry)
            (bind (((name &key initial-value &allow-other-keys) entry))
              (list name initial-value)))
          variable-alist))

(def function variable-bindings/extract-type-declarations (variable-alist)
  (remove nil (mapcar (lambda (entry)
                        (bind (((name &key (type +top-type+) &allow-other-keys) entry))
                          (unless (eql +top-type+ type)
                            `(type ,type ,name))))
                      variable-alist)))

(def function expand ()
  (assert (layer-active-p 'reiterate))
  (bind (((:slots name body variable-bindings/wrapping variable-bindings/loop-body label/top label/next-iteration label/end
                  result-form-candidates symbol-macro-bindings/wrapping macro-bindings/wrapping
                  function-bindings/wrapping inlined-functions
                  exit-conditions/before-loop-body exit-conditions/after-loop-body
                  forms/prologue forms/next-iteration forms/epilogue walk-environment/loop-body) *loop-form*)
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
                           (go ,label/end)))))
      (setf expansion
            `(tagbody
                ,@forms/prologue
              ,label/top
                ,(maybe-wrap-with-bindings (let* variable-bindings/loop-body
                                                :binding-extractor 'variable-bindings/extract-primitive-bindings
                                                :declaration-extractor 'variable-bindings/extract-type-declarations)
                  `(progn
                     ,@(generate-exit-jumps exit-conditions/before-loop-body)
                     ,@body
                     ,@(generate-exit-jumps exit-conditions/after-loop-body)))
              ,label/next-iteration
                ,@forms/next-iteration
                (go ,label/top)
              ,label/end
                ,@forms/epilogue))
      (log.debug "Building result form for ~A" *loop-form*)
      `(block ,name
         ,(maybe-wrap-with-bindings (let* variable-bindings/wrapping
                                          :binding-extractor 'variable-bindings/extract-primitive-bindings
                                          :declaration-extractor 'variable-bindings/extract-type-declarations)
            (maybe-wrap-with-bindings (macrolet macro-bindings/wrapping)
              (maybe-wrap-with-bindings (flet function-bindings/wrapping
                                              :declaration-extractor (lambda (bindings)
                                                                       `((inline ,@(mapcar 'first bindings)))))
                (maybe-wrap-with-bindings (symbol-macrolet symbol-macro-bindings/wrapping)
                  `(progn ,expansion
                          ,result-form)))))))))
