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
  (if *preserve-source-form-identities*
      (bind ((seen (make-hash-table :test 'eq)))
        (labels ((recurse (object)
                   (when (consp object)
                     (when (gethash object seen)
                       (iterate-compile-error "~@<hu.dwim.reiterate relies on the cons cell identities of the source form, and the form you provided repeatedly contains the same identity. You may disable this error by setting the variable ~S to T, but by that you'll also hinder the debugger's ability from properly locating source code from stack frames. The repeated form is ~S.~:>" '*preserve-source-form-identities* object))
                     (setf (gethash object seen) #t)
                     (progn
                       (recurse (car object))
                       (recurse (cdr object))))))
          (recurse whole)))
      ;; this copy-tree hinders the debugger from matching source code to stack frames
      (setf whole (copy-tree whole)))
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
  (bind (((:slots block-name body variable-bindings/wrapping variable-bindings/loop-body label/top label/next-iteration label/end
                  result-form-candidates symbol-macro-bindings/wrapping macro-bindings/wrapping
                  function-bindings/wrapping inlined-functions
                  exit-conditions/before-loop-body exit-conditions/after-loop-body
                  forms/prologue forms/next-iteration forms/epilogue walk-environment/loop-body) *loop-form*)
         (expansion nil)
         (result-form nil))
    (log.debug "Processing toplevel iterate form ~S; stack is ~A" *loop-form* *loop-form-stack*)
    (setf body (mapcar (lambda (el)
                         (log.debug "Walking body form ~S; stack is ~A" el *loop-form-stack*)
                         (bind ((*clause* nil)
                                (walked (walk-form el :parent *loop-form* :environment walk-environment/loop-body)))
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
       (iterate-compile-warning "More than one such clause was used, that can provide a return value. Due to ambiguity the return value will be (VALUES). The form in question is ~S." *loop-form*)))
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
      `(block ,block-name
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
