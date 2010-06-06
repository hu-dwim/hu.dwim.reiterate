;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

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
  (bind (((value &key in into) (rest -clause-))
         (value (-unwalk-form- (-walk-form- value))))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (bind (((variable/head variable/last-cons) (ensure-clause-data (list :collect into)
                                                   (list (register/variable (or into "COLLECT/HEAD") nil)
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
                   (setq ,variable/head ,variable/last-cons)))))))))

(macrolet
    ;; this macro introduces a single variable and promotes it as a return value candidate
    ((single-variable-clause-expander ((variable-name value-form-variable-name initial-value clause-data-key-name
                                                      &key (temporary-variable-name-prefix (string+ (string clause-data-key-name) "/VALUE"))
                                                      (result-form-candidate #f) (type +top-type+))
                                       &body body)
       (with-unique-names (in into name overridden-type)
         `(bind (((,value-form-variable-name &key ((:in ,in)) ((:into ,into))) (rest -clause-)))
            (with-possibly-different-iteration-context (,in :clause -clause-)
              (bind (((:values ,name ,overridden-type) (if ,into
                                                           (extract-variable-name-and-type ,into :default-type ,type)
                                                           (values ,temporary-variable-name-prefix ,type)))
                     (,variable-name (ensure-clause-data (list ,clause-data-key-name ,into)
                                       (register/variable ,name ,initial-value ,overridden-type))))
                ,(when result-form-candidate
                       `(register/result-form-candidate (list ,clause-data-key-name ,name) ,variable-name))
                (maybe-wrap-with-progn (list ,@body))))))))

  (def clause sum
      (clause-of-kind? sum summing)
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t)
      `(incf ,variable ,(-unwalk-form- (-walk-form- value-form)))))

  (def clause count
      (clause-of-kind? count counting)
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t :type 'integer)
      `(when ,(-unwalk-form- (-walk-form- value-form))
         (incf ,variable)))))

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
