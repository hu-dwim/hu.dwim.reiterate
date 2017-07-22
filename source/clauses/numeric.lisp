;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

(def clause repeat
  (clause-of-kind? repeat)
  (progn
    (assert-clause-length 2)
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
      (clause-of-kind? ((sum summing)))
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t)
      `(incf ,variable ,(-recurse- value-form))))

  (def clause count
      (clause-of-kind? ((count counting)))
    (single-variable-clause-expander (variable value-form 0 :sum :result-form-candidate #t :type 'integer)
      `(when ,(-recurse- value-form)
         (incf ,variable)))))

(def function register-generator/numeric-sequence (name-form from to comparator by)
  ;; TODO we could extract a useful type annotation if the values are literal numbers
  (bind (((:values name type) (extract-variable-name-and-type name-form))
         (variable/current (register/variable name :initial-value from :type type))
         (stepper `(setq ,variable/current (+ ,variable/current ,by)))
         (has-more-condition (if to
                                 `(,comparator ,variable/current ,(register/variable "FOR/LIMIT" :initial-value to))
                                 nil)))
    (register/next-iteration-form stepper)
    (register/generator name variable/current stepper :stepper/place has-more-condition :type type)))

(defmacro define-for/from-clause (name matcher destructuring-form from to comparator by iter-context)
  `(def clause ,name
     ,matcher
     (bind ((,destructuring-form (rest -clause-)))
       (with-possibly-different-iteration-context (,iter-context :clause -clause-)
         (expand/generator/has-more-check
          (register-generator/numeric-sequence name ,from ,to ,comparator ,by))))))

;; TODO use STEP instead of BY?
;; TODO make sure clause keywords can be both cl keywords and simple symbols (needs a smarter destructuring bind)

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

(define-for/from-clause for/from/upto
    (clause-of-kind? (for from (to upto)))
  (name _ from _ to &key (by 1) in)
  from
  to
  '<=
  by
  in)

(define-for/from-clause for/from/downto
    (clause-of-kind? (for from downto))
  (name _ from _ downto &key (by -1) in)
  from
  downto
  '>=
  by
  in)

(define-for/from-clause for/from/below
    (clause-of-kind? (for from below))
  (name _ from _ below &key (by 1) in)
  from
  below
  '<
  by
  in)

(define-for/from-clause for/from/above
    (clause-of-kind? (for from above))
  (name _ from _ below &key (by -1) in)
  from
  below
  '>
  by
  in)
