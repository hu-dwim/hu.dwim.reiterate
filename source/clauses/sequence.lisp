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
      (iterate-compile-error "~@<Unable to parse clause ~S~:@>" -clause-))
    (expand/generator/stepper (second -clause-))))

(def function register-generator/in-list (name-form the-list &key (mutable #f) (initially nil initially?))
  (bind (((:values name type) (extract-variable-name-and-type name-form)))
    (if mutable
        (bind ((variable/previous-cons (register/variable "IN-LIST/PREVIOUS-CONS" :type 'cons))
               (variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" :initial-value the-list :type 'list))
               (place `(car ,variable/previous-cons))
               (stepper `(progn
                           (setq ,variable/previous-cons ,variable/current-cons)
                           (setq ,variable/current-cons (cdr ,variable/current-cons)))))
          (apply 'register/generator name place stepper :stepper/place variable/current-cons
                 :mutable mutable :type type
                 (when initially?
                   `(:initial-value ,initially))))
        (bind ((variable/current-cons (register/variable "IN-LIST/CURRENT-CONS" :initial-value the-list :type 'list))
               (place `(car ,variable/current-cons))
               (stepper `(setq ,variable/current-cons (cdr ,variable/current-cons))))
          (apply 'register/generator
                 name place stepper :place/stepper variable/current-cons
                 :mutable mutable :type type
                 (when initially?
                   `(:initial-value ,initially)))))))

(def clause for/in-list
  (named-clause-of-kind? for in-list)
  (bind (((name _ the-list &key (mutable #f) (initially nil initially?)) (rest -clause-))
         (the-list (-recurse- the-list)))
    (expand/generator/stepper
     (apply 'register-generator/in-list name the-list :mutable mutable
            (when initially?
              `(:initially ,(-recurse- initially)))))))

(def clause generate/in-list
  (named-clause-of-kind? generate in-list)
  (bind (((name _ the-list &key mutable (initially nil initially?)) (rest -clause-))
         (the-list (-recurse- the-list)))
    (apply 'register-generator/in-list name the-list :mutable mutable
           (when initially?
             `(:initially ,(-recurse- initially))))
    (values)))

(def function register-generator/in-vector (name-form the-vector &key (mutable #f) (initially nil initially?) start end step)
  (bind (((:values name type) (extract-variable-name-and-type name-form))
         (variable/vector (register/variable "IN-VECTOR/VECTOR" :initial-value the-vector :type 'vector))
         (variable/length (register/variable "IN-VECTOR/LENGTH"
                                             :initial-value (or end
                                                                `(length ,variable/vector))
                                             :type 'array-index))
         (variable/index (register/variable "IN-VECTOR/INDEX" :initial-value start :type 'array-index)))
    (if mutable
        ;; mutable iterators need to keep the previous index so that they can provide a place that
        ;; points to the current value after they have been stepped to the next.
        (bind ((variable/previous-index (register/variable "IN-VECTOR/PREVIOUS-INDEX" :initial-value start :type 'array-index)))
          (apply 'register/generator name
                 `(aref ,variable/vector ,variable/previous-index)
                 `(progn
                    (setf ,variable/previous-index ,variable/index)
                    (incf ,variable/index ,step))
                 :stepper/place
                 `(< ,variable/index ,variable/length)
                 :mutable mutable :type type
                 (when initially?
                   `(:initial-value ,initially))))
        (apply 'register/generator name
               `(aref ,variable/vector ,variable/index)
               `(incf ,variable/index ,step)
               :place/stepper
               `(< ,variable/index ,variable/length)
               :mutable mutable :type type
               (when initially?
                 `(:initial-value ,initially))))))

(def clause for/in-vector
  (named-clause-of-kind? for in-vector)
  (bind (((name _ the-vector &key mutable (initially nil initially?) (start 0) end (step 1)) (rest -clause-))
         (the-vector (-recurse- the-vector)))
    (expand/generator/stepper
     (apply 'register-generator/in-vector name the-vector
            :mutable mutable
            :start (-recurse- start)
            :end (-recurse- end)
            :step (-recurse- step)
            (when initially?
              `(:initially ,(-recurse- initially)))))))

(def clause generate/in-vector
  (named-clause-of-kind? generate in-vector)
  (bind (((name _ the-vector &key mutable (initially nil initially?) (start 0) end (step 1)) (rest -clause-))
         (the-vector (-recurse- the-vector)))
    (apply 'register-generator/in-vector name the-vector
           :mutable mutable
           :start (-recurse- start)
           :end (-recurse- end)
           :step (-recurse- step)
           (when initially?
             `(:initially ,(-recurse- initially))))
    (values)))
