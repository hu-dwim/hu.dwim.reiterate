;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def layer reiterate ()
  ())

(def function walk-form/reiterate (form &key lexical-environment)
  (with-active-layers (reiterate)
    (walk-form form :environment (make-walk-environment lexical-environment))))

(def (definer :available-flags "e") walker/reiterate (name &body body)
  (with-standard-definer-options name
    `(def (walker :in reiterate) ,name
         ,@body)))

(def class* loop-form ()
  ((body)
   (whole)))

(def function make-loop-form (whole)
  (make-instance 'loop-form :body (rest whole) :whole whole))

(def walker/reiterate iter
  (make-loop-form -form-))

(def walker/reiterate iterate
  (make-loop-form -form-))
