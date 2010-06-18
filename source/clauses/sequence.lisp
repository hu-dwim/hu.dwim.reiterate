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
         (the-list (-recurse- the-list)))
    (expand/generator/stepper
     (register-generator/in-list name the-list mutable initially))))

(def clause generate/in-list
  (named-clause-of-kind? generate in-list)
  (bind (((name nil the-list &key mutable initially) (rest -clause-))
         (the-list (-recurse- the-list)))
    (register-generator/in-list name the-list mutable initially)
    (values)))

(def function register-generator/in-vector (name-form the-vector mutable? initial-value start end step)
  (bind (((:values name type) (extract-variable-name-and-type name-form))
         (variable/vector (register/variable "IN-VECTOR/VECTOR" the-vector))
         (variable/length (register/variable "IN-VECTOR/LENGTH" (or end
                                                                    `(length ,variable/vector))))
         (variable/index (register/variable "IN-VECTOR/INDEX" start)))
    (if mutable?
        ;; mutable iterators need to keep the previous index so that they can provide a place that
        ;; points to the current value after they have been stepped to the next.
        (bind ((variable/previous-index (register/variable "IN-VECTOR/PREVIOUS-INDEX" start)))
          (register/generator name
                              `(aref ,variable/vector ,variable/previous-index)
                              `(progn
                                 (setf ,variable/previous-index ,variable/index)
                                 (incf ,variable/index ,step))
                              :stepper/place
                              `(< ,variable/index ,variable/length)
                              :mutable mutable? :type type :initial-value initial-value))
        (register/generator name
                            `(aref ,variable/vector ,variable/index)
                            `(incf ,variable/index ,step)
                            :place/stepper
                            `(< ,variable/index ,variable/length)
                            :mutable mutable? :type type :initial-value initial-value))))

(def clause for/in-vector
  (named-clause-of-kind? for in-vector)
  (bind (((name nil the-vector &key mutable initially (start 0) end (step 1)) (rest -clause-))
         (the-vector (-recurse- the-vector)))
    (expand/generator/stepper
     (register-generator/in-vector name the-vector mutable
                                   (-recurse- initially)
                                   (-recurse- start)
                                   (-recurse- end)
                                   (-recurse- step)))))

(def clause generate/in-vector
  (named-clause-of-kind? generate in-vector)
  (bind (((name nil the-vector &key mutable initially (start 0) end (step 1)) (rest -clause-))
         (the-vector (-recurse- the-vector)))
    (register-generator/in-vector name the-vector mutable
                                  (-recurse- initially)
                                  (-recurse- start)
                                  (-recurse- end)
                                  (-recurse- step))
    (values)))
