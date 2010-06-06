;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def clause next
  (named-clause-of-kind? next)
  (progn
    (unless (and (length= 2 -clause-)
                 (typep (second -clause-) 'variable-name))
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (expand/generator/stepper (second -clause-))))

(def function register-generator/in-list (name-form the-list mutable? initial-value)
  (bind (((:values name type) (extract-variable-name-and-type name-form)))
    (if mutable?
        (bind ((variable/previous-cons (register/variable "IN-LIST/PREVIOUS-CONS" #t))
               (variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" the-list))
               (place `(car ,variable/previous-cons))
               (stepper `(progn
                           (setq ,variable/previous-cons ,variable/current-cons)
                           (setq ,variable/current-cons (cdr ,variable/current-cons)))))
          (register/generator name place stepper :stepper/place variable/current-cons
                              :mutable mutable? :type type :initial-value initial-value))
        (bind ((variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" the-list))
               (place `(car ,variable/current-cons))
               (stepper `(setq ,variable/current-cons (cdr ,variable/current-cons))))
          (register/generator name place stepper :place/stepper variable/current-cons
                              :mutable mutable? :type type :initial-value initial-value)))))

(def clause for/in-list
  (named-clause-of-kind? for in-list)
  (bind (((name nil the-list &key mutable initially) (rest -clause-))
         (the-list (-unwalk-form- (-walk-form- the-list))))
    (expand/generator/stepper
     (register-generator/in-list name the-list mutable initially))))

(def clause generate/in-list
  (named-clause-of-kind? generate in-list)
  (bind (((name nil the-list &key mutable initially) (rest -clause-))
         (the-list (-unwalk-form- (-walk-form- the-list))))
    (register-generator/in-list name the-list mutable initially)
    (values)))

(def function register-generator/in-vector (name-form the-vector mutable? initial-value)
  (bind (((:values name type) (extract-variable-name-and-type name-form))
         (variable/vector  (register/variable "IN-VECTOR/VECTOR" the-vector))
         (variable/length  (register/variable "IN-VECTOR/LENGTH" `(length ,variable/vector))))
    (if mutable?
        (bind ((variable/index   (register/variable "IN-VECTOR/INDEX" 0)))
          (register/generator name
                              `(aref ,variable/vector (1- ,variable/index))
                              `(incf ,variable/index)
                              :stepper/place
                              `(< ,variable/index ,variable/length)
                              :mutable mutable? :type type :initial-value initial-value))
        (bind ((variable/index   (register/variable "IN-VECTOR/INDEX" 0)))
          (register/generator name
                              `(aref ,variable/vector ,variable/index)
                              `(incf ,variable/index)
                              :place/stepper
                              `(< ,variable/index ,variable/length)
                              :mutable mutable? :type type :initial-value initial-value)))))

(def clause for/in-vector
  (named-clause-of-kind? for in-vector)
  (bind (((name nil the-vector &key mutable initially) (rest -clause-))
         (the-vector (-unwalk-form- (-walk-form- the-vector))))
    (expand/generator/stepper
     (register-generator/in-vector name the-vector mutable initially))))

(def clause generate/in-vector
  (named-clause-of-kind? generate in-vector)
  (bind (((name nil the-vector &key mutable initially) (rest -clause-))
         (the-vector (-unwalk-form- (-walk-form- the-vector))))
    (register-generator/in-vector name the-vector mutable initially)
    (values)))
