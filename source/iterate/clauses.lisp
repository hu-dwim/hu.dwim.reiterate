;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate)

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (assert-clause-length 2)
    (bind ((count (-recurse- (second -clause-)))
           (variable (register/variable "REPEAT/COUNTER" :initial-value count)))
      `(progn
         (when (<= ,variable 0)
           (go ,(label/end-of *loop-form*)))
         (decf ,variable)
         (values)))))

(def clause always
  (clause-of-kind? always)
  (progn
    (assert-clause-length 2)
    (bind ((result-variable (register/ensure-result-variable))
           (expr (-recurse- (second -clause-))))
      `(or (setq ,result-variable ,expr)
           (return-from ,(block-name-of *loop-form*) nil)))))

(def clause never
  (clause-of-kind? never)
  (progn
    (assert-clause-length 2)
    (bind ((expr (-recurse- (second -clause-))))
      `(when ,expr
         (return-from ,(block-name-of *loop-form*) nil)))))
