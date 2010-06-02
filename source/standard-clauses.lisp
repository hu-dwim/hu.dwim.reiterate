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
    (expand-to-generator-stepper (second -clause-))))

(def function register-generator/in-list (name the-list mutable?)
  (if mutable?
      (bind ((variable/previous-cons (register/variable "IN-LIST/PREVIOUS-CONS" #t))
             (variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" the-list))
             (place `(car ,variable/previous-cons))
             (stepper `(progn
                         (setq ,variable/previous-cons ,variable/current-cons)
                         (setq ,variable/current-cons (cdr ,variable/current-cons)))))
        (register/generator name place stepper :stepper-place variable/current-cons :mutable mutable?))
      (bind ((variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" the-list))
             (place `(car ,variable/current-cons))
             (stepper `(setq ,variable/current-cons (cdr ,variable/current-cons))))
        (register/generator name place stepper :place-stepper variable/current-cons :mutable mutable?))))

(def clause for/in-list
  (named-clause-of-kind? for in-list)
  (bind (((name nil the-list &key mutable) (rest -clause-))
         (the-list (-unwalk-form- (-walk-form- the-list))))
    (expand-to-generator-stepper
     (register-generator/in-list name the-list mutable))))

(def clause generate/in-list
  (named-clause-of-kind? generate in-list)
  (bind (((name nil the-list &key mutable) (rest -clause-))
         (the-list (-unwalk-form- (-walk-form- the-list))))
    (register-generator/in-list name the-list mutable)
    (values)))

(def function register-generator/in-vector (name the-vector mutable?)
  (bind ((variable/vector  (register/variable "IN-VECTOR/VECTOR" the-vector))
         (variable/length  (register/variable "IN-VECTOR/LENGTH" `(length ,variable/vector))))
    (if mutable?
        (bind ((variable/index   (register/variable "IN-VECTOR/INDEX" 0)))
          (register/generator name
                              `(aref ,variable/vector (1- ,variable/index))
                              `(incf ,variable/index)
                              :stepper-place
                              `(< ,variable/index ,variable/length)
                              :mutable mutable?))
        (bind ((variable/index   (register/variable "IN-VECTOR/INDEX" 0)))
          (register/generator name
                              `(aref ,variable/vector ,variable/index)
                              `(incf ,variable/index)
                              :place-stepper
                              `(< ,variable/index ,variable/length)
                              :mutable mutable?)))))

(def clause for/in-vector
  (named-clause-of-kind? for in-vector)
  (bind (((name nil the-vector &key mutable) (rest -clause-))
         (the-vector (-unwalk-form- (-walk-form- the-vector))))
    (expand-to-generator-stepper
     (register-generator/in-vector name the-vector mutable))))

(def clause generate/in-vector
  (named-clause-of-kind? generate in-vector)
  (bind (((name nil the-vector &key mutable) (rest -clause-))
         (the-vector (-unwalk-form- (-walk-form- the-vector))))
    (register-generator/in-vector name the-vector mutable)
    (values)))

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((count (-walk-form- (second -clause-)))
           (variable (register/variable "REPEAT/COUNTER" (-unwalk-form- count))))
      `(progn
         (when (<= ,variable 0)
           (go ,(end-label-of *loop-form*)))
         (decf ,variable)
         (values)))))

(def clause collect
  (clause-of-kind? collect collecting)
  (progn
    (unless (<= 2 (length -clause-))
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind (((value &key in into) (rest -clause-))
           (value (-unwalk-form- (-walk-form- value))))
      (with-possibly-different-iteration-context (in :clause -clause-)
        (bind (((variable/head variable/last-cons) (ensure-clause-data (list :collect into)
                                                     (list (or into (register/variable "COLLECT/HEAD" nil))
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
                     (setq ,variable/head ,variable/last-cons))))))))))

(def clause sum
  (clause-of-kind? sum summing)
  (clause-expander/single-named-variable (variable/sum 0 :sum :result-form-candidate #t)
    `(incf ,variable/sum ,(-unwalk-form- (-walk-form- value)))))

(def clause count
  (clause-of-kind? count counting)
  (clause-expander/single-named-variable (variable/sum 0 :sum :result-form-candidate #t)
    (with-unique-names (value-tmp)
      `(let ((,value-tmp ,(-unwalk-form- (-walk-form- value))))
         (cond
           ((numberp ,value-tmp)
            (incf ,variable/sum ,value-tmp))
           (,value-tmp
            (incf ,variable/sum 1)))))))

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
