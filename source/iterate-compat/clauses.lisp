;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2017 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/iterate-compat)

;; these are clauses that should be compatible with their counterparts in iterate

(def clause for
  (clause-of-kind? (for in))
  (bind (((name _ the-list &key (generate #f)) (rest -clause-))
         (the-list (-recurse- the-list)))
    (declare (ignore generate)) ; TODO
    (expand/generator/stepper
     (register-generator/in-list name the-list))))

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (assert-clause-length 2)
    (bind ((count (-recurse- (second -clause-)))
           (variable (register/variable "REPEAT/COUNTER" :initial-value count)))
      `(progn
         (when (<= ,variable 0)
           (go ,(label/end-of *loop-form*)))
         (decf ,variable)
         (values)))))

(def clause always
  (clause-of-kind? always)
  (progn
    (assert-clause-length 2)
    (bind ((result-variable (register/ensure-result-variable))
           (expr (-recurse- (second -clause-))))
      `(or (setq ,result-variable ,expr)
           (return-from ,(block-name-of *loop-form*) nil)))))

(def clause never
  (clause-of-kind? never)
  (progn
    (assert-clause-length 2)
    (bind ((expr (-recurse- (second -clause-))))
      `(when ,expr
         (return-from ,(block-name-of *loop-form*) nil)))))

(def clause thereis
  (clause-of-kind? thereis)
  (progn
    (assert-clause-length 2)
    (bind ((result-variable (register/ensure-result-variable))
           (expr (-recurse- (second -clause-))))
      `(when (setq ,result-variable ,expr)
         (return-from ,(block-name-of *loop-form*) ,result-variable)))))

;;; numeric

(define-for/from-clause (for/from :priority -1000)
    (clause-of-kind? (for (from upfrom downfrom)))
  (name from-name from &key
        (by (eswitch (from-name :test 'equal/clause-name)
              ('from
               1)
              ('upfrom
               1)
              ('downfrom
               -1)))
        in)
  from
  nil
  nil
  by
  in)

;;; collecting

(macrolet
    ((x (&rest entries)
       `(progn
          ,@(loop
              :for entry :in entries
              :collect (bind (((clause-name kind &rest keyword-args) (ensure-list entry)))
                         `(def clause ,clause-name
                            (clause-of-kind? ,clause-name)
                            (bind (((value &key in into (at :end) ,@keyword-args) (rest -clause-))
                                   (value (-recurse- value)))
                              (with-possibly-different-iteration-context (in :clause -clause-)
                                (expand/collector into value ',kind :at at
                                                  ,@(mappend (lambda (keyword-arg)
                                                               (list (intern (string (first keyword-arg))
                                                                             (load-time-value (find-package "KEYWORD")))
                                                                     (first keyword-arg)))
                                                             keyword-args))))))))))
  (x (collect collecting)
     ;; appending
     ;; nconcing
     ;; (adjoining (test #'eql))
     ;; (unioning (test #'eql))
     ))
