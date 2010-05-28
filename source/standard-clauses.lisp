;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def clause for/in-list
  (named-clause-of-kind? 'for 'in-list)
  (progn
    (unless (<= 4 (length -clause-) 4)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((the-list (-walk-form- (fourth -clause-)))
           (variable/car (-register- :wrapping-binding nil (second -clause-)))
           (variable/current-cons (-register- :wrapping-binding (-unwalk-form- the-list) "IN-LIST/CURRENT-CONS")))
      (-register- :body-form `(setq ,variable/car (car ,variable/current-cons)))
      (-register- :exit-condition/after-loop-body `(atom (setq ,variable/current-cons (cdr ,variable/current-cons)))))))

(def clause repeat
  (clause-of-kind? 'repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((count (-walk-form- (second -clause-)))
           (variable (-register- :wrapping-binding (-unwalk-form- count) "REPEAT-COUNTER")))
      (-register- :exit-condition/before-loop-body `(zerop ,variable))
      (-register- :body-form `(decf ,variable)))))
