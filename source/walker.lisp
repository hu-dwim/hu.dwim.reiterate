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
  (member thing +toplevel-macro-aliases+ :test #'eq))

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
   (block-name :type loop-name)
   (body)
   (body-conses (make-hash-table :test #'eq))
   (walk-environment/enclosing)
   (walk-environment/loop-body)
   (walk-environment/current :documentation "Contains the currnet walk-environment while descending down on the body forms.")
   (clause-data '() :initarg nil)
   (generators '() :initarg nil)
   (variable-bindings/wrapping '() :initarg nil)
   (variable-bindings/loop-body '() :initarg nil)
   (function-bindings/wrapping '() :initarg nil)
   (inlined-functions '() :initarg nil)
   (macro-bindings/wrapping '() :initarg nil)
   (symbol-macro-bindings/wrapping '() :initarg nil)
   (label/top (generate-unique-name 'top) :initarg nil)
   (label/next-iteration (generate-unique-name 'next) :initarg nil)
   (label/end (generate-unique-name 'end) :initarg nil)
   (result-form :initarg nil)
   (result-form-candidates '() :initarg nil)
   (forms/prologue '() :initarg nil)
   (forms/next-iteration '() :initarg nil)
   (forms/epilogue '() :initarg nil)
   ;; these are not used currently for anything
   (exit-conditions/before-loop-body '() :initarg nil)
   (exit-conditions/after-loop-body '() :initarg nil)))

(def print-object loop-form
  (princ (name-of -self-)))

(def constructor (loop-form name)
  (unless (slot-boundp -self- 'block-name)
    (setf (block-name-of -self-) (gensym (if name
                                             (string name)
                                             "ITER-BLOCK")))))

(def function walk-loop-form (form parent walk-environment)
  "Turn an iterate sexp into a LOOP-FORM CLOS object without walking its body."
  (bind ((name nil)
         (body (rest form)))
    (labels ((assert-proper-name ()
               (unless name
                 (name-error)))
             (name-error ()
               (iterate-compile-error "~@<~S is not a valid name for a loop in form ~S~:>" name form)))
      (when (and body
                 (first body))
        (cond
          ((and (atom (first body))
                (string= (first body) 'named))
           (pop body)
           (setf name (pop body))
           (assert-proper-name))
          ((and (consp (first body))
                (string= (first (first body)) 'named))
           (bind ((clause (pop body)))
             (unless (length= 2 clause)
               (iterate-compile-error "~@<Illegal clause ~S~:>" clause))
             (setf name (second clause))
             (assert-proper-name)))
          ((typep (first body) 'loop-name)
           (setf name (pop body))
           (assert-proper-name)))
        (unless (typep name 'loop-name)
          (name-error))))
    (bind ((block-form (with-form-object (block 'block-form nil :name name)
                         (walk-environment/augment! walk-environment :block name block)))
           (loop-form (with-form-object (*loop-form* 'loop-form parent :whole form :name name :body body
                                                     :result-of-macroexpansion? #t
                                                     :walk-environment/enclosing walk-environment
                                                     :walk-environment/loop-body (walk-environment/copy walk-environment))
                        (flet ((augment (type value)
                                 (walk-environment/augment! (walk-environment/loop-body-of *loop-form*) type value)))
                          (augment :tag (label/top-of *loop-form*))
                          (augment :tag (label/next-iteration-of *loop-form*))
                          (augment :tag (label/end-of *loop-form*)))
                        (bind ((body-conses (body-conses-of *loop-form*)))
                          ;; register which conses are part of our body, so that we can properly handle nested loops later.
                          ;; NOTE: must be careful with macrolets and inner loops inside a backquote; see TEST/NESTING/BUG/1
                          (labels ((recurse (node)
                                     ;; FIXME this descends here too eagerly, e.g. into the expansion of the backquote reader.
                                     ;; (on implementations where it's represented as transparent lists (e.g on CCL), as opposed to e.g. SBCL, where they are opaque structs).
                                     ;; there should be a semantic walker here that only walks code execution contexts.
                                     (unless (or (atom node)
                                                 (reiterate-toplevel-macro-name? (first node))
                                                 (quoted-form? node))
                                       (log.debug "Registering as body of ~A ~S" *loop-form* node)
                                       (setf (gethash node body-conses) #t)
                                       (do ((cons node (cdr cons)))
                                           ((not (consp cons)))
                                         (recurse (car cons))))))
                            (recurse body))))))
      (setf (parent-of block-form) loop-form)
      loop-form)))

(for-each-iterator-alias alias
  `(def walker/reiterate ,alias
     (walk-loop-form -form- -parent- -environment-)))
