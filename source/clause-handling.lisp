;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def namespace clause-handler)

(def function maybe-wrap-with-progn (forms)
  (if (length= 1 forms)
      (first forms)
      `(progn
         ,@forms)))

(def function %register/variable (whole name initial-value)
  (bind (((:slots walk-environment/loop-body) *loop-form*))
    (appendf (wrapping-bindings-of *loop-form*) `((,name ,initial-value)))
    (walk-environment/augment! walk-environment/loop-body :variable name)
    name))

(def function %register/named-variable (whole args)
  (bind ((name (first args))
         (initial-value (second args))
         ((:slots walk-environment/loop-body) *loop-form*))
    (unless (and (<= 1 (length args) 2)
                 (typep name 'variable-name))
      (iterate-compile-error "~S: don't know how to deal with ~S in clause handler ~S" '-register- (list* :variable args) whole))
    (%register/variable whole name initial-value)))

(def function %register/temporary-variable (whole args)
  (bind ((initial-value (first args))
         (name (second args))
         ((:slots walk-environment/loop-body) *loop-form*))
    (unless (and (<= 1 (length args) 2)
                 (stringp name))
      (iterate-compile-error "~S: don't know how to deal with ~S in clause handler ~S" '-register- (list* :temporary-variable args) whole))
    (setf name (generate-unique-name name))
    (%register/variable whole name initial-value)))

(def definer clause (&whole whole-form name match-condition-form expander-form)
  (with-unique-names (whole)
    `(bind ((,whole ',whole-form))
       (macrolet
           ((named-clause-of-kind? (kind &optional sub-kind)
              `(funcall 'named-clause-of-kind? -clause- ,kind ,sub-kind))
            (clause-of-kind? (kind)
              `(funcall 'clause-of-kind? -clause- ,kind)))
         (setf (find-clause-handler ',name)
               (list (named-lambda clause-matcher (-clause-)
                       ,match-condition-form)
                     (named-lambda clause-expander (-clause-)
                       (flet ((-register- (kind &rest args)
                                (ecase kind
                                  (:body-form
                                   (appendf (forms/loop-body-of *loop-form*) args))
                                  (:exit-condition/before-loop-body
                                   (appendf (exit-conditions/before-loop-body-of *loop-form*) args))
                                  (:exit-condition/after-loop-body
                                   (appendf (exit-conditions/after-loop-body-of *loop-form*) args))
                                  (:variable ; (... name initial-value)
                                   (%register/named-variable ,whole args))
                                  (:temporary-variable ; (... initial-value name-hint)
                                   (%register/temporary-variable ,whole args))))
                              (-walk-form- (node &optional (parent *loop-form*) (environment (walk-environment/loop-body-of *loop-form*)))
                                (walk-form node :parent parent :environment environment))
                              (-unwalk-form- (node)
                                (unwalk-form node)))
                         (declare (ignorable #'-register- #'-walk-form- #'-unwalk-form-))
                         ,expander-form))))))))

(def type variable-name ()
  `(and symbol (not (member nil t))))

(def function equal/clause-name (a b)
  (or (eq a b)
      (eq a (find-symbol (string b) :keyword))
      (eq (find-symbol (string a) :keyword) b)))

(def function clause-of-kind? (clause kind)
  (equal/clause-name kind (first clause)))

(def function named-clause-of-kind? (clause kind &optional sub-kind)
  (and (equal/clause-name kind (first clause))
       (typep (second clause) 'variable-name)
       (or (null sub-kind)
           (equal/clause-name sub-kind (third clause)))))

(def function process-clause (form)
  (dolist (clause-handler (collect-namespace-values 'clause-handler))
    (bind (((matcher expander) clause-handler))
      (when (funcall matcher form)
        (funcall expander form)
        (return-from process-clause))))
  (bind ((walked-form (walk-form form :parent *loop-form* :environment (walk-environment/loop-body-of *loop-form*))))
    (appendf (forms/loop-body-of *loop-form*) (list (unwalk-form walked-form)))))
