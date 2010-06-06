;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def type variable-name ()
  `(and symbol (not (member nil t))))

(def function extract-variable-name-and-type (name-form &key (otherwise :error otherwise?))
  (etypecase name-form
    (cons
     (if (and (eq 'the (first name-form))
              (length= 3 name-form)
              (typep (third name-form) 'variable-name))
         (values (third name-form) (second name-form))
         (handle-otherwise
           (iterate-compile-error "Expecting a variable name, possibly inside a ~S form specifying its type, but found ~S" 'the name-form))))
    (variable-name
     (values name-form +top-type+))))

(def function maybe-wrap-with-progn (forms)
  (if (length= 1 forms)
      (first forms)
      `(progn
         ,@forms)))

(def function maybe-wrap-with-type-check (type form)
  (if (eq type +top-type+)
      form
      `(the ,type ,form)))
