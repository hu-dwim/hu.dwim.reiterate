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

(def test test/lexenv/bug/1 ()
  ;; the problem is this: the FOR clause introduces the new variable FOO in the LOOP-BODY walk env,
  ;; but it can not introduce it into the rest of the subtree starting with its parent's sibling, because
  ;; the walk env is immutable and augmentation copies it as the walking descends down.
  (not-signals warning
    (with-expected-failures
      (is (equal '(0 1 2)
                 (eval '(iter (progn
                                (for foo :from 0 :to 2)
                                (collecting foo)))))))
    (is (equal '(0 1 2)
               (eval '(iter (for foo :from 0 :to 2)
                            (collecting foo)))))))

(def test test/lexenv/clauses-dont-hide-current ()
  (not-signals warning
    (is (equal '(42 42 42)
               (eval '(iter (symbol-macrolet ((i 42))
                              (for i :from 0 :to 2)
                              (collecting i))))))))

(def test test/lexenv/clauses-are-walking-in-proper-env ()
  (not-signals warning
    (is (equal 14
               (eval '(iter (for i :from 1 :to 3)
                            (symbol-macrolet ((x (* i i)))
                              (sum x))))))
    (is (equal 14
               (eval '(iter (for i :from 1 :to 3)
                            (macrolet ((x ()
                                         `(* i i)))
                              (sum (x)))))))
    (is (equal 14
               (eval '(iter (for i :from 1 :to 3)
                            (flet ((x ()
                                     (* i i)))
                              (sum (x)))))))))
