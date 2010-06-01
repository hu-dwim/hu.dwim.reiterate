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
      `(bind ((,entry (assoc-value (clause-data-storage-of *loop-form*) ,key :test #'equal)))
         (or ,entry
             (progn
               (log.debug "Registering new clause data with key ~S in loop ~A" ,key *loop-form*)
               (setf (assoc-value (clause-data-storage-of *loop-form*) ,key :test #'equal)
                     (progn ,@value))))))))

(def macro clause-expander/single-named-variable ((variable-name initial-value clause-data-key-name
                                                                 &key (temporary-variable-name-prefix (string+ (string clause-data-key-name) "/VALUE"))
                                                                 (result-form-candidate #f))
                                                   &body body)
  `(bind (((value &key in into) (rest -clause-)))
     (with-possibly-different-iteration-context (in :clause -clause-)
       (bind ((,variable-name (ensure-clause-data (list ,clause-data-key-name into)
                                (or into (register/variable ,temporary-variable-name-prefix ,initial-value)))))
         ,(when result-form-candidate
            `(register/result-form-candidate (list ,clause-data-key-name into) ,variable-name))
         (maybe-wrap-with-progn (list ,@body))))))

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

(def function maybe-wrap-with-progn (forms)
  (if (length= 1 forms)
      (first forms)
      `(progn
         ,@forms)))

(def function expand-to-generator-stepper (name &key (mutable #f))
  (bind (((:values stepper has-more-condition place) (lookup/generator name)))
    (if mutable
        (not-yet-implemented)
        (progn
          (register/variable name)
          `(progn
             (unless ,has-more-condition
               ;; TODO replace -loop-end- inside returned forms?
               (go ,(end-label-of *loop-form*)))
             (prog1
                 (setq ,name ,place)
               ,stepper))))))

(def (function e) register/generator (name place stepper has-more-condition)
  (setf (assoc-value (generators-of *loop-form*) name)
        (list :stepper stepper
              :has-more-condition has-more-condition
              :place place))
  name)

(def (function e) lookup/generator (name)
  (bind ((generator (assoc-value (generators-of *loop-form*) name))
         ((&key stepper has-more-condition place) generator))
    (unless generator
      (iterate-compile-error "Could not find generator ~S" name))
    (values stepper has-more-condition place)))

(def function register/variable (name &optional (initial-value nil))
  (bind (((:slots walk-environment/loop-body) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (wrapping-bindings-of *loop-form*) `((,name ,initial-value)))
    (walk-environment/augment! walk-environment/loop-body :variable name)
    (log.debug "Augmented environment with variable ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
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

(def type variable-name ()
  `(and symbol (not (member nil t))))

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
       (typep (second clause) 'variable-name)
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
