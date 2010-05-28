;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def type loop-name ()
  `(and symbol (not (member t))))

(def layer reiterate ()
  ())

(def function walk-form/reiterate (form &optional lexenv)
  (with-active-layers (reiterate)
    (walk-form form :environment (make-walk-environment lexenv))))

(def function walk-form/reiterate/toplevel (form &optional lexenv)
  (assert (member (first form) '(iter iterate)))
  (walk-form/reiterate (cons 'iterate (rest form)) lexenv))

(def (definer :available-flags "e") walker/reiterate (name &body body)
  (with-standard-definer-options name
    `(def (walker :in reiterate) ,name
         ,@body)))

(def function generate-unique-name (&optional base)
  (bind ((base (string base)))
    (unless (ends-with #\- base)
      (setf base (string+ base "/")))
    (gensym (string base))))

(def class* loop-form (walked-form)
  ((whole)
   (name nil :type loop-name)
   (body)
   (walk-environment/enclosing)
   (walk-environment/loop-body)
   (wrapping-bindings '() :initarg nil)
   (top-label (generate-unique-name 'loop-top) :initarg nil)
   (end-label (generate-unique-name 'loop-end) :initarg nil)
   (result-form '(values) :initarg nil)
   (exit-conditions/before-loop-body '() :initarg nil)
   (exit-conditions/after-loop-body '() :initarg nil)
   (forms/prologue '() :initarg nil)
   (forms/loop-body '() :initarg nil)
   (forms/epilogue '() :initarg nil)))

(def walker/reiterate iterate
  (bind ((name nil)
         (whole -form-)
         (body (rest -form-)))
    (flet ((name-error ()
             (iterate-compile-error "~S is not a valid name for a loop in form ~S" name whole)))
      (when (and body
                 (first body))
        (cond
          ((and (atom (first body))
                (string= (first body) 'named))
           (pop body)
           (setf name (pop body))
           (unless name
             (name-error)))
          ((typep (first body) 'loop-name)
           (setf name (pop body))))
        (unless (typep name 'loop-name)
          (name-error))))
    (make-instance 'loop-form :parent -parent- :whole whole :name name :body body
                   :walk-environment/enclosing -environment-
                   :walk-environment/loop-body (hu.dwim.walker::copy-walk-environment -environment-))))
