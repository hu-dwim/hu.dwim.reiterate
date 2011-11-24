;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/numeric :in test))

(def test test/numeric/for-from-to ()
  (is (equal '(5 7 9)
             (eval '(iter (for foo :from 5 to 10 :by 2)
                          (collecting foo)))))
  (is (equal '(5 6 7)
             (eval '(iter (for foo :from 5 to 7)
                          (collecting foo)))))
  (is (equal '()
             (eval '(iter (for foo :from 5 :to 4)
                          (collecting foo))))))

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

(def test test/numeric/for-from-downto ()
  (is (equal 14
             (eval '(iter (for i :from -1 :downto -3)
                          (symbol-macrolet ((x (* i i)))
                            (summing x)))))))
