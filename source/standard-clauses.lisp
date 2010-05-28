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
      (-register- :body-form `(setq ,variable/car (car ,variable/current-cons)))
      (-register- :exit-condition/after-loop-body `(atom (setq ,variable/current-cons (cdr ,variable/current-cons)))))))

(def clause repeat
  (clause-of-kind? 'repeat)
  (progn
    (unless (length= 2 -clause-)
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((count (-walk-form- (second -clause-)))
           (variable (-register- :temporary-variable (-unwalk-form- count) "REPEAT/COUNTER")))
      (-register- :exit-condition/before-loop-body `(zerop ,variable))
      (-register- :body-form `(decf ,variable)))))

(def clause collect
  (clause-of-kind? 'collect)
  (progn
    (unless (<= 2 (length -clause-))
      (iterate-compile-error "Unable to parse clause ~S" -clause-))
    (bind ((value (-walk-form- (second -clause-)))
           (keys (rest (rest -clause-)))
           (variable/head (-register- :temporary-variable nil "COLLECT/HEAD"))
           (variable/last-cons (-register- :temporary-variable nil "COLLECT/LAST-CONS")))
      (unless (zerop (length keys))
        (iterate-compile-error "Unknown options for clause ~S" -clause-))
      (setf (result-form-of *loop-form*) variable/head) ; TODO what if there's something there already?
      (with-unique-names (value-tmp cons-tmp)
        (-register- :body-form `(let ((,value-tmp ,(-unwalk-form- value)))
                                  (if ,variable/head
                                      (let ((,cons-tmp (cons ,value-tmp nil)))
                                        (setf (cdr ,variable/last-cons) ,cons-tmp)
                                        (setq ,variable/last-cons ,cons-tmp))
                                      (progn
                                        (setq ,variable/last-cons (cons ,value-tmp nil))
                                        (setq ,variable/head ,variable/last-cons)))))))))
