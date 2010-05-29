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

(def function reiterate-toplevel-macro-name? (thing)
  (member thing '(iter iterate) :test #'eq))

(def function walk-iterate-form (whole &optional lexenv)
  (with-active-layers (reiterate)
    (walk-form (if (eq 'iterate (first whole))
                   whole
                   (cons 'iterate (rest whole)))
               :environment (make-walk-environment lexenv))))

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
   (body-conses (make-hash-table :test #'eq))
   (walk-environment/enclosing)
   (walk-environment/loop-body)
   (wrapping-bindings '() :initarg nil)
   (top-label (generate-unique-name 'loop-top) :initarg nil)
   (end-label (generate-unique-name 'loop-end) :initarg nil)
   (result-form :initarg nil)
   (result-form-candidates '() :initarg nil)
   (exit-conditions/before-loop-body '() :initarg nil)
   (exit-conditions/after-loop-body '() :initarg nil)
   (forms/prologue '() :initarg nil)
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
    (with-form-object (*loop-form* 'loop-form -parent- :whole whole :name name :body body
                                   :walk-environment/enclosing -environment-
                                   :walk-environment/loop-body (hu.dwim.walker::copy-walk-environment -environment-))
      (bind ((body-conses (body-conses-of *loop-form*)))
        ;; register which conses are part of our body, so that we can properly handle nested usage later
        (labels ((recurse (node)
                   (unless (or (atom node)
                               (reiterate-toplevel-macro-name? (first node)))
                     (log.debug "Registering as body of ~A ~S" *loop-form* node)
                     (setf (gethash node body-conses) #t)
                     (map nil #'recurse node))))
          (recurse body))))))
