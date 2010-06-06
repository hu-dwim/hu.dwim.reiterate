;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def type variable-name ()
  `(and symbol (not (member nil t))))

(def function extract-variable-name-and-type (name-form &key (otherwise :error otherwise?) (default-type +top-type+))
  (etypecase name-form
    (cons
     (if (and (eq 'the (first name-form))
              (length= 3 name-form)
              (typep (third name-form) 'variable-name))
         (values (third name-form) (second name-form))
         (handle-otherwise
           (iterate-compile-error "Expecting a variable name, possibly inside a ~S form specifying its type, but found ~S" 'the name-form))))
    (variable-name
     (values name-form default-type))))

(def function maybe-wrap-with-progn (forms)
  (if (length= 1 forms)
      (first forms)
      `(progn
         ,@forms)))

(def function maybe-wrap-with-type-check (type form)
  (if (eq type +top-type+)
      form
      `(the ,type ,form)))

(def function %maybe-wrap-with-bindings-fn (binder bindings body binding-extractor declaration-extractor)
  (if bindings
      `(,binder (,@(funcall binding-extractor bindings))
         ,@(bind ((declarations (funcall declaration-extractor bindings)))
             (when declarations
               `((declare ,@declarations))))
         ,body)
      body))

(def macro maybe-wrap-with-bindings ((binder bindings &key (binding-extractor ''identity)
                                             (declaration-extractor '(constantly '())))
                                      &body body)
  (assert (length= 1 body))
  `(%maybe-wrap-with-bindings-fn ',binder ,bindings ,(first body) ,binding-extractor ,declaration-extractor))
