;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def clause initially
  (clause-of-kind? initially)
  (register/prologue (maybe-wrap-with-progn (rest -clause-))))

(def clause finally
  (clause-of-kind? finally)
  (register/epilogue (-unwalk-form- (-walk-form- (maybe-wrap-with-progn (rest -clause-))))))

(def clause next-iteration
  (clause-of-kind? next-iteration)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      `(go ,(label/next-iteration-of *loop-form*)))))

(def clause first-time?
  (clause-of-kind? first-time?)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind ((variable/flag (register/variable "FIRST-TIME/FLAG" #t)))
        `(prog1
             ,variable/flag
           (setq ,variable/flag #f))))))

(def clause first-iteration?
  (clause-of-kind? first-iteration?)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind ((variable (ensure-clause-data 'first-iteration?
                         (bind ((variable (register/variable "FIRST-ITERATION?" #t)))
                           (register/next-iteration-form `(setf ,variable #f))
                           variable))))
        variable))))
