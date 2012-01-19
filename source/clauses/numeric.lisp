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
      (iterate-compile-error "~@<Unable to parse clause ~S~:@>" -clause-))
    (bind ((count (-recurse- (second -clause-)))
           (variable (register/variable "REPEAT/COUNTER" :initial-value count :type 'non-negative-integer)))
      `(progn
         (when (<= ,variable 0)
           (go ,(label/end-of *loop-form*)))
         (decf ,variable)
         (values)))))

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
                                       (register/variable ,name :initial-value ,initial-value :type ,overridden-type))))
                ,(when result-form-candidate
                       `(register/result-form-candidate (list ,clause-data-key-name ,name) ,variable-name))
                (maybe-wrap-with-progn (list ,@body))))))))

  (def clause sum
      (clause-of-kind? sum summing)
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t)
      `(incf ,variable ,(-recurse- value-form))))

  (def clause count
      (clause-of-kind? count counting)
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t :type 'integer)
      `(when ,(-recurse- value-form)
         (incf ,variable)))))

(def function register-generator/numeric-sequence (name-form from to comparator by)
  ;; TODO we could extract a useful type annotation if the values are literal numbers
  (bind (((:values name type) (extract-variable-name-and-type name-form))
         (variable/current (register/variable name :initial-value from :type type))
         (variable/limit (register/variable "FOR/LIMIT" :initial-value to))
         (stepper `(setq ,variable/current (+ ,variable/current ,by)))
         (has-more-condition `(,comparator ,variable/current ,variable/limit)))
    (register/next-iteration-form stepper)
    (register/generator name variable/current stepper :stepper/place has-more-condition :type type)))

(def clause for/from/upto
  (or (named-clause-of-kind? for from to)
      (named-clause-of-kind? for from upto))
  (bind (((name nil from nil to &key (by 1) in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/generator/has-more-check
       (register-generator/numeric-sequence name
                                            (-recurse- from)
                                            (-recurse- to)
                                            '<=
                                            by)))))

(def clause for/from/below
  (named-clause-of-kind? for from below)
  (bind (((name nil from nil below &key (by 1) in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/generator/has-more-check
       (register-generator/numeric-sequence name
                                            (-recurse- from)
                                            (-recurse- below)
                                            '<
                                            by)))))

(def clause for/from/downto
  (named-clause-of-kind? for from downto)
  (bind (((name nil from nil downto &key (by -1) in) (rest -clause-)))
    (with-possibly-different-iteration-context (in :clause -clause-)
      (expand/generator/has-more-check
       (register-generator/numeric-sequence name
                                            (-recurse- from)
                                            (-recurse- downto)
                                            '>=
                                            (-recurse- by))))))
