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
           (variable/car (-register- :variable (second -clause-)))
           (variable/current-cons (-register- :temporary-variable (-unwalk-form- the-list) "IN-LIST/CURRENT-CONS")))
      (-register- :exit-condition/after-loop-body `(atom (setq ,variable/current-cons (cdr ,variable/current-cons))))
      `(setq ,variable/car (car ,variable/current-cons)))))

(def clause repeat
  (clause-of-kind? 'repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((count (-walk-form- (second -clause-)))
           (variable (-register- :temporary-variable (-unwalk-form- count) "REPEAT/COUNTER")))
      (-register- :exit-condition/before-loop-body `(zerop ,variable))
      `(decf ,variable))))

(def clause collect
  (clause-of-kind? 'collect)
  (progn
    (unless (<= 2 (length -clause-))
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind (((value &key in into) (rest -clause-)))
      ;; TODO implement collecting in parent iters
      (declare (ignore in))
      (bind (((variable/head variable/last-cons) (ensure-clause-data (list :collect into)
                                                   (list (or into (-register- :temporary-variable nil "COLLECT/HEAD"))
                                                         (-register- :temporary-variable nil "COLLECT/LAST-CONS"))))
             ;; NOTE: we walk after registering the variables above
             (value (-walk-form- value)))
       (-register- :result-form-candidate (list :collect into) variable/head)
       (with-unique-names (value-tmp cons-tmp)
         `(let ((,value-tmp ,(-unwalk-form- value)))
            (if ,variable/head
                (let ((,cons-tmp (cons ,value-tmp nil)))
                  (setf (cdr ,variable/last-cons) ,cons-tmp)
                  (setq ,variable/last-cons ,cons-tmp))
                (progn
                  (setq ,variable/last-cons (cons ,value-tmp nil))
                  (setq ,variable/head ,variable/last-cons)))))))))

(def clause finally
  (clause-of-kind? 'finally)
  (progn
    (if (slot-boundp *loop-form* 'result-form)
        (iterate-compile-error "Duplicate finally clauses? The result form is already ~S while processing clause ~S" (result-form-of *loop-form*) -clause-)
        (setf (result-form-of *loop-form*) (maybe-wrap-with-progn (rest -clause-))))
    (values)))
