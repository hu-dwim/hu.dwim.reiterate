;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def namespace clause-handler)

(def macro ensure-clause-data (key &body value)
  (with-unique-names (entry)
    (once-only (key)
      `(bind ((,entry (assoc-value (clause-data-of *loop-form*) ,key :test #'equal)))
         (or ,entry
             (progn
               (log.debug "Registering new clause data with key ~S in loop ~A" ,key *loop-form*)
               (setf (assoc-value (clause-data-of *loop-form*) ,key :test #'equal)
                     (progn ,@value))))))))

(def with-macro* with-different-iteration-context (position)
  (bind ((*loop-form* (elt *loop-form-stack* position))
         (*loop-form-stack* (subseq *loop-form-stack* position)))
    (log.debug "Hijacked clause stack is ~A" *loop-form-stack*)
    (multiple-value-prog1
        (-with-macro/body-)
      (log.debug "Hijacking ends"))))

(def with-macro* with-possibly-different-iteration-context (name-or-position &key clause)
  (if name-or-position
      (progn
        (log.debug "Will try to process clause ~S in loop called ~S" clause name-or-position)
        (bind ((position (etypecase name-or-position
                           (number name-or-position)
                           (symbol (or (position name-or-position *loop-form-stack* :key 'name-of :test 'equal)
                                       (iterate-compile-error "Could not find loop called ~S~:[~:; used in clause ~S~]" name-or-position clause clause))))))
          (with-different-iteration-context (position)
            (-with-macro/body-))))
      (-with-macro/body-)))

(def function expand-to-generator-stepper (name)
  (bind (((&key place stepper has-more-condition variable stepper-place-order &allow-other-keys) (lookup/generator name)))
    `(progn
       (unless ,has-more-condition
         ;; TODO replace -loop-end- inside returned forms?
         (go ,(end-label-of *loop-form*)))
       ,@(ecase stepper-place-order
           (:place-stepper `((prog1
                                 (setq ,variable ,place)
                               ,stepper)))
           (:stepper-place `(,stepper
                             (setq ,variable ,place)))))))

(def (function e) register/generator (name place stepper stepper-place-order has-more-condition &key (mutable #f)
                                           (type +top-type+))
  (check-type stepper-place-order (member :stepper-place :place-stepper))
  (bind ((variable/value nil))
    (if mutable
        (with-unique-names (new-value)
          (setf variable/value (register/variable* (string name) :scope :wrapping))
          (register/symbol-macro name `(,name))
          (register/function name () `(,(maybe-wrap-with-type-check type place)) :inline #t)
          (register/function `(setf ,name) `(,new-value) `((setf ,place ,(maybe-wrap-with-type-check type new-value))) :inline #t))
        (setf variable/value (register/variable name nil type)))
    (setf (assoc-value (generators-of *loop-form*) name)
          (list :place place
                :type type
                :stepper stepper
                :stepper-place-order stepper-place-order
                :has-more-condition has-more-condition
                :mutable mutable ; not strictly needed, kept for debugging clarity
                :variable variable/value)))
  name)

(def (function e) lookup/generator (name)
  (bind ((generator (assoc-value (generators-of *loop-form*) name)))
    (unless generator
      (iterate-compile-error "Could not find generator ~S" name))
    generator))

(def (function e) register/variable (name &optional (initial-value nil) (type +top-type+))
  (register/variable* name :initial-value initial-value :type type))

(def (function e) register/variable* (name &key (type +top-type+) (initial-value nil) (scope :wrapping))
  (bind (((:slots walk-environment/loop-body) *loop-form*)
         (storage-slot-name (ecase scope
                              (:wrapping 'variable-bindings/wrapping)
                              (:body 'variable-bindings/loop-body))))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (slot-value *loop-form* storage-slot-name) `((,name :initial-value ,initial-value :type ,type)))
    (walk-environment/augment! walk-environment/loop-body :variable name)
    (log.debug "Augmented environment with variable ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
    name))

(def (function e) register/function (name args body &key inline)
  (when inline
    (pushnew name (inlined-functions-of *loop-form*)))
  (bind (((:slots walk-environment/loop-body) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (function-bindings/wrapping-of *loop-form*) `((,name ,args ,@body)))
    (walk-environment/augment! walk-environment/loop-body :function name)
    (log.debug "Augmented environment with function ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
    name))

(def (function e) register/macro (name args body)
  (bind (((:slots walk-environment/enclosing walk-environment/loop-body) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (macro-bindings/wrapping-of *loop-form*) `((,name ,args ,@body)))
    (walk-environment/augment! walk-environment/loop-body :macro name
                               (hu.dwim.walker::parse-macro-definition name args body
                                                                       (walk-environment/lexical-environment walk-environment/enclosing)))
    (log.debug "Augmented environment with macro ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
    name))

(def (function e) register/symbol-macro (name expansion)
  (bind (((:slots walk-environment/loop-body) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (symbol-macro-bindings/wrapping-of *loop-form*) `((,name ,expansion)))
    (walk-environment/augment! walk-environment/loop-body :symbol-macro name expansion)
    (log.debug "Augmented environment with symbol-macro ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
    name))

(def (function e) register/result-form (result-form)
  (log.debug "Registering result-form ~S, stack is ~A" result-form *loop-form-stack*)
  (when (slot-boundp *loop-form* 'result-form)
    (iterate-compile-error "The result form of ~A is already ~S while processing clause ~S" *loop-form* (result-form-of *loop-form*) *clause*))
  (setf (result-form-of *loop-form*) result-form))

(def (function e) register/result-form-candidate (name value-form)
  (log.debug "Registering result-form-candidate with key ~S, form ~S, stack is ~A" name value-form *loop-form-stack*)
  (setf (assoc-value (result-form-candidates-of *loop-form*) name :test 'equal) value-form))

(def (function e) register/prologue (form)
 (appendf (forms/prologue-of *loop-form*) (list form))
 (values))

(def (function e) register/epilogue (form)
 (appendf (forms/epilogue-of *loop-form*) (list form))
 (values))

(def definer clause (name match-condition-form expander-form)
  `(macrolet
       ((named-clause-of-kind? (kind &optional sub-kind)
          `(funcall 'named-clause-of-kind? -clause- ',kind ',sub-kind))
        (clause-of-kind? (&rest kinds)
          `(or ,@(loop
                   :for kind :in kinds
                   :collect `(funcall 'clause-of-kind? -clause- ',kind)))))
     (setf (find-clause-handler ',name)
           (list (named-lambda clause-matcher (-clause-)
                   ,match-condition-form)
                 (named-lambda clause-expander (-clause-)
                   (flet ((-walk-form- (node &optional (parent *loop-form*) (environment (walk-environment/loop-body-of *loop-form*)))
                            (check-type parent walked-form)
                            (log.debug "Will walk ~S in context ~A" node *loop-form*)
                            (walk-form node :parent parent :environment environment))
                          (-unwalk-form- (node)
                            (unwalk-form node)))
                     (declare (ignorable #'-walk-form- #'-unwalk-form-))
                     ,expander-form))))))

(def function equal/clause-name (a b)
  (or (eq a b)
      (and (symbolp b)
           (eq a (find-symbol (string b) :keyword)))
      (and (symbolp a)
           (eq (find-symbol (string a) :keyword) b))))

(def function clause-of-kind? (clause kind)
  (equal/clause-name kind (first clause)))

(def function named-clause-of-kind? (clause kind &optional sub-kind)
  (and (equal/clause-name kind (first clause))
       (extract-variable-name-and-type (second clause) :otherwise #f)
       (or (null sub-kind)
           (equal/clause-name sub-kind (third clause)))))

(def layered-method walk-form/compound :in reiterate :around (name form parent environment)
  (flet ((loop-stack-position (form)
           ;; finds the first loop-form on the stack that owns this form
           (if (boundp '*loop-form-stack*)
               (progn
                 (log.debug "BELONGS-TO-A-PARENT-ITERATE-FORM? will test with stack ~A" (rest *loop-form-stack*))
                 (position-if (lambda (loop-form)
                                (bind ((result (gethash form (body-conses-of loop-form))))
                                  (log.debug "BELONGS-TO-A-PARENT-ITERATE-FORM? ~S ~A loop-form ~A" form loop-form result)
                                  result))
                              *loop-form-stack*
                              :from-end #t))
               nil)))
    (dolist (clause-handler (collect-namespace-values 'clause-handler))
      (bind (((matcher expander) clause-handler))
        (when (funcall matcher form)
          (bind ((*clause* form))
            (log.debug "Form ~S matched as a clause in stack ~A" form *loop-form-stack*)
            (with-possibly-different-iteration-context ((loop-stack-position form) :clause form)
              (bind ((result (multiple-value-list (funcall expander form))))
                (log.debug "Expanded ~S into ~S" form result)
                (setf result (make-instance 'unwalked-form :source (if (length= 0 result)
                                                                       '(values)
                                                                       (first result))))
                (return-from walk-form/compound result))))))))
  (call-next-layered-method))
