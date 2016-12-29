;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate/test)

(def suite* (test/numeric :in test))

(def test test/numeric/for-from-upto ()
  (is (equal '(5 7 9)
             (eval '(iter (for foo from 5 to 10 :by 2)
                          (collecting foo)))))
  (with-expected-failures
    ;; TODO fix the clause destructuring-bind (and create variations)
    (is (equal '(5 7 9)
               (eval '(iter (for foo from 5 to 10 by 2)
                            (collecting foo))))))
  (is (equal '(5 6 7)
             (eval '(iter (for foo :from 5 upto 7)
                          (collecting foo)))))
  (is (equal '()
             ;; from/to shouldn't even try to be smart about the direction, not even with constants
             (eval '(iter (for foo :from 5 :to 4)
                          (collecting foo))))))

(def test test/numeric/for-from-downto ()
  (is (equal '(10 9 8 7)
             (eval '(iter (for foo :from 10 :downto 7)
                          (collecting foo)))))
  (is (equal 14
             (eval '(iter (for i :from -1 :downto -3)
                          (symbol-macrolet ((x (* i i)))
                            (summing x)))))))

(def test test/numeric/for-from-below ()
  (is (equal '(5 7 9)
             (eval '(iter (for foo :from 5 :below 10 :by 2)
                          (collecting foo)))))
  (is (equal '(5 6)
             (eval '(iter (for foo :from 5 :below 7)
                          (collecting foo)))))
  (is (equal '()
             (eval '(iter (for foo :from 5 :below 4)
                          (collecting foo))))))

(def test test/numeric/for-from-above ()
  (is (equal '(10 8 6)
             (eval '(iter (for foo :from 10 :above 5 :by -2)
                          (collecting foo)))))
  (is (equal '(7 6)
             (eval '(iter (for foo :from 7 :above 5)
                          (collecting foo)))))
  (is (equal '()
             (eval '(iter (for foo :from 4 :above 5)
                          (collecting foo))))))

(def test test/numeric/for-from/without-limit ()
  (is (equal '(5 7 9)
             (eval '(iter (for foo :from 5 :by 2)
                          (repeat 3)
                          (collecting foo)))))
  (is (equal '(10 8 6)
             (eval '(iter (for foo :from 10 :by -2)
                          (repeat 3)
                          (collecting foo)))))
  (is (equal '(10 9 8)
             (eval '(iter (for foo :downfrom 10)
                          (repeat 3)
                          (collecting foo))))))

(def test test/numeric/for-from/nesting ()
  (signals iterate-compile-error
    ;; clause args shouldn't get walked and thus the nested ITER's shouldn't see the outer ones
    (macroexpand-all
     '(iter named outer
       (for foo :from 5 :to 9 :by (progn
                                    (iter (repeat 1)
                                          (collecting 42 :in outer))
                                    2))
       (repeat 1)))))
