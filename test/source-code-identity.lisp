;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/source-code-identity :in test))

(def test test/source-code-identity/forms-without-clauses ()
  (bind ((body-forms `((+ 2 3)
                       (progn
                         42)
                       (list 1 2 3)))
         (expansion (cl:macroexpand `(iter (repeat 1)
                                           ,@body-forms))))
    (dolist (body-form body-forms)
      (is (find/tree body-form expansion)))))

(def test test/source-code-identity/same-multiple-times ()
  (bind ((body-form `(+ i i)))
    (signals iterate-compile-error
      (cl:macroexpand `(iter (for i :from 0 :to 3)
                             (collecting ,body-form)
                             (collecting ,body-form))))
    (signals iterate-compile-error
      (cl:macroexpand `(iter (for i :from 0 :to 3)
                             (collecting ,body-form)
                             ,body-form)))))

(def test test/source-code-identity/macroexpansion-not-done-unnecessarily ()
  (bind ((body-form `(macrolet ((foo ()
                                  `(+ i 40)))
                       (foo))))
    (bind ((expansion (finishes
                        (cl:macroexpand `(iter (for i :from 2 :to 100)
                                               (leave (unless nil
                                                        ,body-form)))))))
      (is (equal 42 (eval expansion)))
      (is (find/tree body-form expansion)))))

(def test test/source-code-identity/nested-same-clause-identity ()
  (bind ((clause '(collecting 42)))
    ;; without proper handling of CONS identities, it may erronously return '(42 42) if the nested COLLECTING runs in the outer
    (is (equal '(42)
               (eval `(iter (repeat 1)
                            ,clause
                            (iter (repeat 1)
                                  ,clause)))))))
