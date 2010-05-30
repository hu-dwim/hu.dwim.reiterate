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
    (expand-to-generator-stepper (second -clause-) :whole -clause-)))

(def clause for/in-list
  (named-clause-of-kind? for in-list)
  (progn
    (unless (<= 4 (length -clause-) 4)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((the-list (-walk-form- (fourth -clause-)))
           (variable/car (second -clause-))
           (variable/current-cons (-register- :variable "IN-LIST/CURRENT-CONS" (-unwalk-form- the-list))))
      (expand-to-generator-stepper
       (-register- :generator variable/car
                   `(prog1
                        (setq ,variable/car (car ,variable/current-cons))
                      (setq ,variable/current-cons (cdr ,variable/current-cons)))
                   variable/current-cons)
       :whole -clause-))))

(def clause for/in-vector
  (named-clause-of-kind? for in-vector)
  (progn
    (unless (<= 4 (length -clause-) 4)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((the-vector (fourth -clause-))
           (variable/element (second -clause-))
           (variable/vector  (-register- :variable "IN-VECTOR/VECTOR" (-unwalk-form- (-walk-form- the-vector))))
           (variable/length  (-register- :variable "IN-VECTOR/LENGTH" `(length ,variable/vector)))
           (variable/index   (-register- :variable "IN-VECTOR/INDEX" 0)))
      (expand-to-generator-stepper
       (-register- :generator variable/element
                   `(prog1
                        (setq ,variable/element (aref ,variable/vector ,variable/index))
                      (incf ,variable/index))
                   `(< ,variable/index ,variable/length))
       :whole -clause-))))

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((count (-walk-form- (second -clause-)))
           (variable (-register- :variable "REPEAT/COUNTER" (-unwalk-form- count))))
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
    (bind (((value &key in into) (rest -clause-)))
      (with-possibly-different-iteration-context (in :clause -clause-)
        (bind (((variable/head variable/last-cons) (ensure-clause-data (list :collect into)
                                                     (list (or into (-register- :variable "COLLECT/HEAD" nil))
                                                           (-register- :variable "COLLECT/LAST-CONS" nil)))))
          (-register- :result-form-candidate (list :collect into) variable/head)
          (with-unique-names (value-tmp cons-tmp)
            `(let ((,value-tmp ,(-unwalk-form- (-walk-form- value))))
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
  (-register- :prologue (maybe-wrap-with-progn (rest -clause-))))

(def clause finally
  (clause-of-kind? finally)
  (-register- :epilogue (maybe-wrap-with-progn (rest -clause-))))

(def clause first-time?
  (clause-of-kind? first-time?)
  (bind (((&key in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind ((variable/flag (-register- :variable "FIRST-TIME/FLAG" #t)))
        `(prog1
             ,variable/flag
           (setq ,variable/flag #f))))))
