;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/types :in test))

(def test test/types/for-in-list ()
  ;; problem: the binding of I is introduced with the initial value of NIL which is not of type FIXNUM.
  (is (equalp '(2 3 4)
              (eval '(iter (for (the fixnum i) :in-list '(1 2 3))
                      (collecting (1+ i))))))
  (is (equalp '(2 0 11 3 10 21)
              (eval '(iter (for (the fixnum i) :in-list '(1 2) :initially 0)
                           (generate (the fixnum j) :in-list '(10 20) :initially 0)
                           (collecting (1+ i))
                           (collecting j)
                           (collecting (1+ (next j))))))))

(def test test/types/for-in-vector ()
  ;; problem: same as above
  (is (equalp '(2 3 4)
              (eval '(iter (for (the fixnum i) :in-vector #(1 2 3))
                      (collecting (1+ i))))))
  (is (equalp '(2 0 11 3 10 21)
              (eval '(iter (for (the fixnum i) :in-vector #(1 2) :initially 0)
                           (generate (the fixnum j) :in-vector #(10 20) :initially 0)
                           (collecting (1+ i))
                           (collecting j)
                           (collecting (1+ (next j))))))))
