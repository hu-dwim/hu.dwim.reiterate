;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def clause collect
  (clause-of-kind? collect collecting)
  (bind (((value &key in into) (rest -clause-))
         (value (-unwalk-form- (-walk-form- value))))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind (((variable/head variable/last-cons) (ensure-clause-data (list :collect into)
                                                   (list (register/variable (or into "COLLECT/HEAD") nil)
                                                         (register/variable "COLLECT/LAST-CONS" nil)))))
        (register/result-form-candidate (list :collect into) variable/head)
        (with-unique-names (value-tmp cons-tmp)
          `(let ((,value-tmp ,value))
             (if ,variable/head
                 (let ((,cons-tmp (cons ,value-tmp nil)))
                   (setf (cdr ,variable/last-cons) ,cons-tmp)
                   (setq ,variable/last-cons ,cons-tmp))
                 (progn
                   (setq ,variable/last-cons (cons ,value-tmp nil))
                   (setq ,variable/head ,variable/last-cons)))))))))

(def clause initially
  (clause-of-kind? initially)
  (register/prologue (maybe-wrap-with-progn (rest -clause-))))

(def clause finally
  (clause-of-kind? finally)
  (register/epilogue (maybe-wrap-with-progn (rest -clause-))))

(def clause first-time?
  (clause-of-kind? first-time?)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind ((variable/flag (register/variable "FIRST-TIME/FLAG" #t)))
        `(prog1
             ,variable/flag
           (setq ,variable/flag #f))))))
