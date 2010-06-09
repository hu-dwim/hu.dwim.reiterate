;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

;; TODO nconcing, appending, adjoining, unioning

(def function expand/collector (name value-form kind)
  (bind (((variable/head variable/last-cons) (ensure-clause-data (list 'collecting name)
                                               (list (register/variable (or name "COLLECTING/HEAD") nil)
                                                     (register/variable "COLLECTING/LAST-CONS" nil)))))
    (register/result-form-candidate (list 'collecting name) variable/head)
    (with-unique-names (value)
      `(let ((,value ,value-form))
         ,(ecase kind
             (collect
               `(setq ,variable/last-cons (if ,variable/head
                                              (setf (cdr ,variable/last-cons) (cons ,value nil))
                                              (setq ,variable/head (cons ,value nil)))))
             (append
               `(progn
                  (setq ,value (copy-list ,value))
                  (setq ,variable/last-cons (last (if ,variable/head
                                                      (setf (cdr ,variable/last-cons) ,value)
                                                      (setq ,variable/head ,value)))))))))))

(def clause collecting
  (clause-of-kind? collecting)
  (bind (((value &key in into) (rest -clause-))
         (value (-unwalk-form- (-walk-form- value))))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/collector into value 'collect))))

(def clause appending
  (clause-of-kind? appending)
  (bind (((value &key in into) (rest -clause-))
         (value (-unwalk-form- (-walk-form- value))))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/collector into value 'append))))
