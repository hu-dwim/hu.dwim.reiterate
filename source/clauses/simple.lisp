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
  (register/epilogue (-recurse- (maybe-wrap-with-progn (rest -clause-)))))

;; a direct return-from
(def clause leave
  (clause-of-kind? leave)
  (bind (((&optional return-value &key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      `(return-from ,(block-name-of *loop-form*) ,return-value))))

;; FINISH runs the epilogue, just like exiting due to a WHILE clause
(def clause finish
  (clause-of-kind? finish)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/finish-loop-when #t))))

(def clause next-iteration
  (clause-of-kind? next-iteration)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      `(go ,(label/next-iteration-of *loop-form*)))))

(def clause while
  (clause-of-kind? while)
  (bind (((condition &key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/finish-loop-when `(not ,(-recurse- condition))))))

(def clause until
  (clause-of-kind? until)
  (bind (((condition &key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/finish-loop-when (-recurse- condition)))))

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
