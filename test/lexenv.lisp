;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/lexenv :in test))

(def special-variable *dummy-special*)

(def test test/lexenv/iteration-variables-show-up-in-lexenv ()
  (with-captured-lexical-environment
      (env (iter (for x :in-list '(1))
                 (for y :in-vector #(1))
                 -here-))
    (is (find-variable-in-lexenv 'x env))
    (is (find-variable-in-lexenv 'y env))))

(def test test/lexenv/enclosing-environment-works-fine ()
  (with-captured-lexical-environment
      (env (let ((x 42))
             (declare (special x))
             (symbol-macrolet ((dummy-symbol-macro 42))
               (macrolet ((dummy-macro ()))
                 (iter (for x :in-list '(1))
                       (for *dummy-special* :in-vector #(1))
                       -here-)))))
    (is (find-symbol-macro-in-lexenv 'dummy-symbol-macro env))
    (is (find-macro-in-lexenv 'dummy-macro env))
    (is (find-variable-in-lexenv 'x env))
    (is (find-variable-in-lexenv '*dummy-special* env))
    (is (not (special-variable-name? 'x env)))
    (is (proclaimed-special-variable?/lexical '*dummy-special* env))))
