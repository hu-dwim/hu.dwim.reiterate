;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/basic :in test))

(def test test/basic/naming ()
  (is (eq (name-of (walk-iterate-form '(iter alma)))
          'alma))
  (is (eq (name-of (walk-iterate-form '(iter named alma)))
          'alma))
  (signals error (walk-iterate-form '(iter named))))

(def test test/basic/clauses-expand ()
  (flet ((check (form)
           (finishes (macroexpand form))))
    (check '(iter (repeat 2)
                  (print 42)))
    (check '(iter (repeat 2)
                  (for i :in-list '(1 2 3))
                  (collecting i)))
    (check '(iter (:repeat 2)
                  (for (the string i) in-list '(1 2 3))
                  (:collecting i)
                  (finally (return 42))))
    (check '(iter (for (the fixnum i) :in-vector '(1 2 3))
                  (:collecting i)
                  (finally (return 42))))))

(def test test/basic/collecting ()
  (is (equal '(1 1 1)
             (eval '(iter (repeat 3)
                          (collecting 1)))))
  (is (equal '(1 2 3)
             (eval '(iter (for i :in-list '(1 2 3))
                          (collecting i)))))
  (is (equal '(1 1 2 2 3 3)
             (eval '(iter (for i :in-list '(1 2 3))
                          (collecting i)
                          (collecting i)))))
  (is (equal '((A) A (B B) B B (C) C)
             (eval '(iter (for x :in-list '((a) (b b) (c)))
                          (collecting x)
                          (appending x))))))

(def test test/basic/scoping ()
  (is (= 6 (eval '(let ((foo '(1 2 3)))
                   (iter (for foo :in-list foo)
                         (summing foo)))))))

(def test test/basic/sum ()
  (is (= 10 (eval '(iter (repeat 9.5) (sum 1)))))
  (signals error (eval '(locally
                            (declare #+sbcl(sb-ext:muffle-conditions warning))
                          (iter (repeat 1) (sum 'not-a-number))))))

(def test test/basic/count ()
  (is (= 10 (eval '(iter (repeat 9.5) (counting t)))))
  (is (= 9  (eval '(iter (repeat 9) (counting 2)))))
  (is (= 0  (eval '(iter (repeat -1.5) (counting t)))))
  (is (= 2  (eval '(iter (for i :in-list '(1 2 3))
                         (counting (oddp i) :into result)
                         (finally (return result)))))))

(def test test/basic/initially ()
  (is (equal '(42)
             (eval '(let ((x 10))
                      (iter (initially (setf x 42))
                            (repeat 1)
                            (collecting x)))))))

(def test test/basic/finally ()
  (is (= 42 (eval '(iter named alma
                    (repeat 1)
                    (finally (return-from alma 42))))))
  (is (equal '(101 42 42) (eval '(iter
                                  (repeat 2)
                                  (collecting 42 :into x)
                                  (progn
                                    (finally (return (list* 101 x)))))))))

(def test test/basic/first-time? ()
  (is (equal '("x" "," "x" "," "x")
             (eval '(iter (repeat 3)
                          (unless (first-time?)
                            (collecting ","))
                          (collecting "x")))))
  (is (equal '((    "x" "," "x")
               ("," "x" "," "x"))
             (eval '(iter named outer
                          (repeat 2)
                          (collecting (iter (repeat 2)
                                            (unless (first-time? :in outer)
                                              (collecting ","))
                                            (collecting "x"))))))))

(def test test/basic/first-iteration? ()
  (is (equal '(t nil nil)
             (eval '(iter (repeat 3)
                          (collecting (first-iteration?)))))))

(def test test/basic/for/in-list ()
  (is (equal '(a x b y)
             (eval '(iter (for i :in-list '(a b c))
                          (for j :in-list '(x y))
                          (collecting i)
                          (collecting j)))))
  (is (equal '(1 2)
             (eval '(iter outer
                          (repeat 1)
                          (iter (collecting (for x :in-list '(1 2)) :in outer))))))
  (is (equal '((1 2 3) (2 3 4))
             (eval '(let ((data (list 1 2 3)))
                     (list (iter (for x :in-list data :mutable t)
                                 (collecting x)
                                 (incf x))
                            data))))))

(def test test/basic/gnerate/in-list ()
  (is (equal '(a a x b b y c c)
             (eval '(iter (generate i :in-list '(a b c))
                          (generate j :in-list '(x y))
                          (collecting (next i))
                          (collecting i)
                          (collecting (next j))))))
  (is (equal '(1 2)
             (eval '(iter outer
                          (repeat 1)
                          (iter (generate x :in-list '(1 2))
                                (collecting (next x) :in outer))))))
  (is (equal '((1 1 2 2 3 3) (2 3 4))
             (eval '(let ((data (list 1 2 3)))
                     (list (iter (collecting (progn
                                               (generate x :in-list data :mutable t)
                                               (next x)))
                                 (collecting x)
                                 (incf x))
                            data))))))

(def test test/basic/for/in-vector ()
  (is (equalp '(a x b y)
              (eval '(iter (for i :in-vector #(a b c))
                           (for j :in-vector #(x y))
                           (collecting i)
                           (collecting j)))))
  (is (equalp '(1 2)
              (eval '(iter outer
                           (repeat 1)
                           (iter (collecting (for x :in-vector #(1 2)) :in outer))))))
  (is (equalp '((1 2 3) #(2 3 4))
              (eval '(let ((data (vector 1 2 3)))
                       (list (iter (for x :in-vector data :mutable t)
                                   (collecting x)
                                   (incf x))
                             data)))))
  (is (equalp '(2 4 6)
              (eval '(iter (for i :in-vector #(1 2 3 4 5 6 7 8) :start 1 :end 7 :step 2)
                           (collecting i)))))
  (is (equalp '((2 4 6)
                #(1 2 3 4 5 6 7 8))
              (eval '(let ((vector (vector 1 2 3 4 5 6 7 8)))
                       (list (iter (for i :in-vector vector :start 1 :end 7 :step 2)
                                   (collecting i)
                                   (setf i (1+ i)))
                             vector)))))
  (is (equalp '((2 4 7 11)
                #(1 4 3 16 5 6 49 8 9 10 121 12))
              (eval '(let ((vector (vector 1 2 3 4 5 6 7 8 9 10 11 12))
                           (step 1))
                      (list (iter (for i :in-vector vector :start 1 :end 11 :step (setf step (1+ step)) :mutable t)
                                  (collecting i)
                                  (setf i (* i i)))
                       vector))))))

(def test test/basic/generate/in-vector ()
  (is (equalp '(a a x b b y c c)
              (eval '(iter (generate i :in-vector #(a b c))
                           (generate j :in-vector #(x y))
                           (collecting (next i))
                           (collecting i)
                           (collecting (next j))))))
  (is (equalp '(1 2)
              (eval '(iter outer
                           (repeat 1)
                           (iter (collecting (progn
                                               (generate x :in-vector #(1 2))
                                               (next x))
                                   :in outer))))))
  (is (equalp '((1 1 2 2 3 3) #(2 3 4))
              (eval '(let ((data (vector 1 2 3)))
                       (list (iter (collecting (progn
                                                 (generate x :in-vector data :mutable t)
                                                 (next x)))
                                   (collecting x)
                                   (incf x))
                             data)))))
  (is (equalp '(2 4 6)
              (eval '(iter (generate i :in-vector #(1 2 3 4 5 6 7 8) :start 1 :end 7 :step 2)
                           (collecting (next i))))))
  (is (equalp '((2 4 6)
                #(1 2 3 4 5 6 7 8))
              (eval '(let ((vector (vector 1 2 3 4 5 6 7 8)))
                       (list (iter (generate i :in-vector vector :start 1 :end 7 :step 2)
                                   (next i)
                                   (collecting i)
                                   (setf i (1+ i)))
                             vector)))))
  (is (equalp '((2 4 7 11)
                #(1 4 3 16 5 6 49 8 9 10 121 12))
              (eval '(let ((vector (vector 1 2 3 4 5 6 7 8 9 10 11 12))
                           (step 1))
                      (list (iter (generate i :in-vector vector :start 1 :end 11 :step (setf step (1+ step)) :mutable t)
                                  (next i)
                                  (collecting i)
                                  (setf i (* i i)))
                       vector))))))
