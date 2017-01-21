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

;; TODO ALWAYS requires support for a one-and-only the-result-variable
(def clause always
  (clause-of-kind? always)
  (progn
    (assert-clause-length 2)
    (bind ((expr (-recurse- (second -clause-)))
           (result-variable (register/variable "ALWAYS/RESULT" :initial-value #t)))
      (register/result-form result-variable)
      `(or (setq ,result-variable ,expr)
           (return-from ,(block-name-of *loop-form*) nil)))))
