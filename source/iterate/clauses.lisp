;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate)

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "~@<Unable to parse clause ~S~:@>" -clause-))
    (bind ((count (-recurse- (second -clause-)))
           (variable (register/variable "REPEAT/COUNTER" :initial-value count)))
      `(progn
         (when (<= ,variable 0)
           (go ,(label/end-of *loop-form*)))
         (decf ,variable)
         (values)))))
