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

(def macro assert-clause-length (expected-length &body other-conditions)
  `(unless (and (length= ,expected-length -clause-)
                ,@other-conditions)
     (iterate-compile-error "~@<Unable to parse clause ~S~:>" -clause-)))

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
                                       (iterate-compile-error "~@<Could not find loop called ~S~:[~:; used in clause ~S~]~:>" name-or-position clause clause))))))
          (with-different-iteration-context (position)
            (-with-macro/body-))))
      (-with-macro/body-)))

(def (function e) expand/generator/stepper (name)
  (bind (((&key place stepper variable stepper-place-order &allow-other-keys) (lookup/generator name)))
    `(progn
       ,@(awhen (expand/generator/has-more-check name)
           (list it))
       ,@(bind ((assign-form (if (eq place variable)
                                 variable
                                 `(setq ,variable ,place))))
           (ecase stepper-place-order
             (:place/stepper
              `((prog1
                    ,assign-form
                  ,stepper)))
             (:stepper/place
              `(,stepper
                ,assign-form)))))))

(def function expand/finish-loop-when (condition)
  (bind ((leave-form `(go ,(label/end-of *loop-form*))))
    (if (eq condition #t)
        leave-form
        `(when ,condition
           ,leave-form))))

(def (function e) expand/generator/has-more-check (name)
  (bind (((&key has-more-condition &allow-other-keys) (lookup/generator name)))
    (if has-more-condition
        (expand/finish-loop-when `(not ,has-more-condition))
        (values))))

(def (function e) register/generator (name place stepper stepper-place-order has-more-condition &key (mutable #f)
                                           (type +top-type+) (initial-value (initial-value-for-type type)))
  (check-type stepper-place-order (member :stepper/place :place/stepper))
  (bind ((variable/value nil))
    (if mutable
        (with-unique-names (new-value)
          (setf variable/value (register/variable (string name) :initial-value initial-value :type type))
          (register/symbol-macro name `(,name))
          (register/function name () `(,(maybe-wrap-with-type-check type place)) :inline #t)
          (register/function `(setf ,name) `(,new-value) `((setf ,place ,(maybe-wrap-with-type-check type new-value))) :inline #t))
        (setf variable/value (if (consp place)
                                 (register/variable name :initial-value initial-value :type type)
                                 place)))
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
      (iterate-compile-error "~@<Could not find generator ~S~:>" name))
    generator))

(def (function e) register/variable (name &key (type +top-type+) (initial-value (initial-value-for-type type)) (scope :wrapping))
  (bind (((:slots walk-environment/loop-body walk-environment/current) *loop-form*)
         (storage-slot-name (ecase scope
                              (:wrapping 'variable-bindings/wrapping)
                              (:body 'variable-bindings/loop-body))))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (slot-value *loop-form* storage-slot-name) `((,name :initial-value ,initial-value :type ,type)))
    (walk-environment/augment! walk-environment/loop-body :variable name)
    (walk-environment/augment! walk-environment/current :variable name)
    (log.debug "Augmented environment with variable ~S in the context of ~A" name *loop-form*)
    ;; (log.debug "WALK-ENVIRONMENT/LOOP-BODY is ~A" walk-environment/loop-body)
    ;; (log.debug "WALK-ENVIRONMENT/CURRENT is ~A" walk-environment/current)
    name))

(def (function e) register/function (name args body &key inline)
  (when inline
    (pushnew name (inlined-functions-of *loop-form*)))
  (bind (((:slots walk-environment/loop-body walk-environment/current) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (function-bindings/wrapping-of *loop-form*) `((,name ,args ,@body)))
    (walk-environment/augment! walk-environment/loop-body :function name)
    (walk-environment/augment! walk-environment/current :function name)
    (log.debug "Augmented environment with function ~S in the context of ~A" name *loop-form*)
    name))

(def (function e) register/macro (name args body)
  (bind (((:slots walk-environment/enclosing walk-environment/loop-body walk-environment/current) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (macro-bindings/wrapping-of *loop-form*) `((,name ,args ,@body)))
    (bind ((parsed-macro (hu.dwim.walker::parse-macro-definition name args body
                                                                 (walk-environment/lexical-environment walk-environment/enclosing))))
      (walk-environment/augment! walk-environment/loop-body :macro name parsed-macro)
      (walk-environment/augment! walk-environment/current :macro name parsed-macro))
    (log.debug "Augmented environment with macro ~S in the context of ~A" name *loop-form*)
    name))

(def (function e) register/symbol-macro (name expansion)
  (bind (((:slots walk-environment/loop-body walk-environment/current) *loop-form*))
    (when (stringp name)
      (setf name (generate-unique-name name)))
    (appendf (symbol-macro-bindings/wrapping-of *loop-form*) `((,name ,expansion)))
    (walk-environment/augment! walk-environment/loop-body :symbol-macro name expansion)
    (walk-environment/augment! walk-environment/current :symbol-macro name expansion)
    (log.debug "Augmented environment with symbol-macro ~S in the context of ~A" name *loop-form*)
    name))

(def (function e) register/result-form (result-form)
  (log.debug "Registering result-form ~S, stack is ~A" result-form *loop-form-stack*)
  (when (slot-boundp *loop-form* 'result-form)
    (iterate-compile-error "~@<The result form of ~A is already ~S while processing clause ~S~:>" *loop-form* (result-form-of *loop-form*) *clause*))
  (setf (result-form-of *loop-form*) result-form))

(def (function e) register/result-form-candidate (name value-form)
  (log.debug "Registering result-form-candidate with key ~S, form ~S, stack is ~A" name value-form *loop-form-stack*)
  (setf (assoc-value (result-form-candidates-of *loop-form*) name :test 'equal) value-form))

(def (function e) register/prologue (form)
  (appendf (forms/prologue-of *loop-form*) (list form))
  (values))

(def (function e) register/next-iteration-form (form)
  (appendf (forms/next-iteration-of *loop-form*) (list form))
  (values))

(def (function e) register/epilogue (form)
  (appendf (forms/epilogue-of *loop-form*) (list form))
  (values))

(def (definer :available-flags "e") clause (name-and-options match-condition-form expander-form)
  (bind (((name &key (priority 0)) (ensure-list name-and-options)))
    (with-standard-definer-options name
      `(macrolet
           ((named-clause-of-kind? (&rest args)
              `(funcall 'named-clause-of-kind? -clause- ,@(mapcar (lambda (el) `(quote ,el)) args)))
            (clause-of-kind? (&rest kinds)
              `(or ,@(loop
                       :for kind :in kinds
                       :collect `(funcall 'clause-of-kind? -clause- ',kind)))))
         (setf (find-clause-handler ',name)
               (list ,priority
                     (named-lambda clause-matcher (-clause-)
                       ,match-condition-form)
                     (named-lambda clause-expander (-clause-)
                       (flet ((-recurse- (node &optional (parent *loop-form*) (environment (walk-environment/current-of *loop-form*)))
                                (check-type parent walked-form)
                                (log.debug "Will walk ~S in context ~A" node *loop-form*)
                                (bind ((walked-form (walk-form node :parent parent :environment environment)))
                                  (unwalk-form walked-form))))
                         (declare (ignorable #'-recurse-))
                         ,expander-form))))))))

(def function equal/clause-name (a b)
  (or (eq a b)
      (and (symbolp b)
           (eq a (find-symbol (string b) :keyword)))
      (and (symbolp a)
           (eq (find-symbol (string a) :keyword) b))))

(def function clause-of-kind? (clause kind)
  (equal/clause-name kind (first clause)))

(def function named-clause-of-kind? (clause kind &optional sub-kind1 sub-kind2)
  (and (equal/clause-name kind (first clause))
       (extract-variable-name-and-type (second clause) :otherwise #f)
       (or (null sub-kind1)
           (some (lambda (valid)
                   (equal/clause-name valid (third clause)))
                 (ensure-list sub-kind1)))
       (or (null sub-kind2)
           (some (lambda (valid)
                   (equal/clause-name valid (fifth clause)))
                 (ensure-list sub-kind2)))))

;; used as a marker that we have something down in the AST that is the result of a clause expansion
(def class* unwalked-clause-form (unwalked-form)
  ())

(def layered-method unwalk-form :in reiterate :around (form)
  (if (or (boundp 'unconditionally-unwalk?)
          *clause* ; if we are inside a clause, then returning the original form is not an option
          (result-of-macroexpansion? form)
          (typep form 'unwalked-clause-form) ; shortcut for the map-ast below, to generate less log noise
          (block finding-loop-form
            (map-ast (lambda (child-form)
                       (when (typep child-form '(or loop-form unwalked-clause-form))
                         (log.debug "Not skipping unwalk because of child node: ~A. Source is ~S" child-form (source-of form))
                         (return-from finding-loop-form #t))
                       child-form)
                     form)
            #f))
      (bind ((unconditionally-unwalk? #t)) ; to skip repeated unnecessary map-ast calls
        (declare (special unconditionally-unwalk?))
        (call-next-layered-method))
      (progn
        (log.debug "Seems like there were no clauses, so we return the original source ~S of form ~A" (source-of form) form)
        (source-of form))))

(def layered-method walk-form/compound :in reiterate :around (name form parent environment)
  (declare (ignore name parent))
  (when (boundp '*loop-form*)
    (setf (walk-environment/current-of *loop-form*) environment))
  (flet ((loop-stack-position (form)
           ;; finds the first loop-form on the stack that owns this form
           (if (boundp '*loop-form-stack*)
               (progn
                 (log.debug "LOOP-STACK-POSITION will search stack ~A for form ~A" *loop-form-stack* form)
                 (bind ((position (position-if (lambda (loop-form)
                                                 (bind ((result (gethash form (body-conses-of loop-form))))
                                                   ;; NOTE re the log output: POSITION-IF is not obliged to call us as few times as possible,
                                                   ;; i.e. it is allowed to traverse from the left, even when :FROM-END T is specified.
                                                   (log.dribble "BELONGS-TO-A-PARENT-ITERATE-FORM? form: ~S loop-form: ~A result: ~A" form loop-form result)
                                                   result))
                                               *loop-form-stack*
                                               :from-end #t)))
                   (log.debug "LOOP-STACK-POSITION is returning with ~A" position)
                   ;; TODO this assert should be alive here, but that requires fixing the walker not to walk into non-body
                   ;; contexts (e.g. into the expansion of the backquote reader)
                   ;; (assert (integerp position))
                   position))
               nil)))
    ;; SORT by priority, which is the FIRST in the entry list.
    (dolist (clause-handler (sort (collect-namespace-values 'clause-handler)
                                  #'>
                                  :key #'first))
      (bind (((_ matcher expander) clause-handler))
        (when (funcall matcher form)
          (bind ((*clause* form))
            (log.debug "Form ~S matched as a clause in stack ~A" form *loop-form-stack*)
            (with-possibly-different-iteration-context ((loop-stack-position form) :clause form)
              (bind ((result (multiple-value-list (funcall expander form))))
                (log.debug "Expanded ~S into ~S" form result)
                (setf result (make-instance 'unwalked-clause-form :source (if (length= 0 result)
                                                                              '(values)
                                                                              (first result))))
                (return-from walk-form/compound result))))))))
  (call-next-layered-method))
