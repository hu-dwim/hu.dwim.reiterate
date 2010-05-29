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
                                (or into (-register- :temporary-variable ,initial-value ,temporary-variable-name-prefix)))))
         ,(when result-form-candidate
            `(-register- :result-form-candidate (list ,clause-data-key-name into) ,variable-name))
         (-walk-form- (maybe-wrap-with-progn (list ,@body)))))))

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

(def function %register/variable (name initial-value)
  (bind (((:slots walk-environment/loop-body) *loop-form*))
    (appendf (wrapping-bindings-of *loop-form*) `((,name ,initial-value)))
    (walk-environment/augment! walk-environment/loop-body :variable name)
    (log.debug "Augmented environment with variable ~S in the context of ~A: ~A" name *loop-form* walk-environment/loop-body)
    name))

(def function %register/named-variable (whole args)
  (bind ((name (first args))
         (initial-value (second args))
         ((:slots walk-environment/loop-body) *loop-form*))
    (unless (and (<= 1 (length args) 2)
                 (typep name 'variable-name))
      (iterate-compile-error "~S: don't know how to deal with ~S in clause handler ~S" '-register- (list* :variable args) whole))
    (%register/variable name initial-value)))

(def function %register/temporary-variable (whole args)
  (bind ((initial-value (first args))
         (name (second args))
         ((:slots walk-environment/loop-body) *loop-form*))
    (unless (and (<= 1 (length args) 2)
                 (stringp name))
      (iterate-compile-error "~S: don't know how to deal with ~S in clause handler ~S" '-register- (list* :temporary-variable args) whole))
    (setf name (generate-unique-name name))
    (%register/variable name initial-value)))

(def function %register/result-form (whole args)
  (unless (length= 1 args)
    (iterate-compile-error "~S: result form should be one form in clause handler ~S" '-register- whole))
  (bind ((result-form (first args)))
    (log.debug "Registering result-form ~S, stack is ~A" result-form *loop-form-stack*)
    (when (slot-boundp *loop-form* 'result-form)
      (iterate-compile-error "The result form of ~A is already ~S while processing clause ~S" *loop-form* (result-form-of *loop-form*) whole))
    (setf (result-form-of *loop-form*) result-form)))

(def function %register/result-form-candidate (whole args)
  (bind ((name (first args))
         (value (second args)))
    (unless (length= 2 args)
      (iterate-compile-error "~S: don't know how to deal with ~S in clause handler ~S" '-register- (list* :result-form-candidate args) whole))
    (log.debug "Registering result-form-candidate with key ~S, stack is ~A" name *loop-form-stack*)
    (setf (assoc-value (result-form-candidates-of *loop-form*) name :test 'equal) value)))

(def definer clause (&whole whole-form name match-condition-form expander-form)
  (with-unique-names (whole)
    `(bind ((,whole ',whole-form))
       (macrolet
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
                       (flet ((-register- (kind &rest args)
                                (ecase kind
                                  (:prologue
                                   (appendf (forms/prologue-of *loop-form*) args)
                                   (values))
                                  (:epilogue
                                   (appendf (forms/epilogue-of *loop-form*) args)
                                   (values))
                                  (:exit-condition/before-loop-body
                                   (appendf (exit-conditions/before-loop-body-of *loop-form*) args)
                                   (values))
                                  (:exit-condition/after-loop-body
                                   (appendf (exit-conditions/after-loop-body-of *loop-form*) args)
                                   (values))
                                  (:result-form
                                   (%register/result-form ,whole args)
                                   (values))
                                  (:result-form-candidate ; (... name value)
                                   (%register/result-form-candidate ,whole args)
                                   (values))
                                  (:variable ; (... name initial-value)
                                   (%register/named-variable ,whole args))
                                  (:temporary-variable ; (... initial-value name-hint)
                                   (%register/temporary-variable ,whole args))))
                              (-walk-form- (node &optional (parent *loop-form*) (environment (walk-environment/loop-body-of *loop-form*)))
                                (log.debug "Will walk ~S in context ~A" node *loop-form*)
                                (walk-form node :parent parent :environment environment))
                              (-unwalk-form- (node)
                                (unwalk-form node)))
                         (declare (ignorable #'-register- #'-walk-form- #'-unwalk-form-))
                         ,expander-form))))))))

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
          (log.debug "Form ~S matched as a clause in stack ~A" form *loop-form-stack*)
          (with-possibly-different-iteration-context ((loop-stack-position form) :clause form)
            (bind ((result (multiple-value-list (funcall expander form))))
              (log.debug "Expanded ~S into ~S" form result)
              (setf result (if (length= 0 result)
                               (make-instance 'free-application-form :operator 'values :arguments '())
                               (first result)))
              (check-type result walked-form)
              (return-from walk-form/compound result)))))))
  (call-next-layered-method))
