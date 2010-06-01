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
  (finishes (macroexpand '(iter (repeat 2)
                                (print 42))))
  (finishes (macroexpand '(iter (repeat 2)
                                (for i :in-list '(1 2 3))
                                (collect i))))
  (finishes (macroexpand '(iter (:repeat 2)
                                (for i in-list '(1 2 3))
                                (:collect i)
                                (finally (return 42))))))

(def test test/basic/collect ()
  (is (equal '(1 1 1)
             (eval '(iter (repeat 3)
                          (collect 1)))))
  (is (equal '(1 2 3)
             (eval '(iter (for i :in-list '(1 2 3))
                          (collect i)))))
  (is (equal '(1 1 2 2 3 3)
             (eval '(iter (for i :in-list '(1 2 3))
                          (collect i)
                          (collect i))))))

(def test test/basic/sum ()
  (is (= 10 (eval '(iter (repeat 9.5) (sum 1)))))
  (signals error (eval '(locally
                            (declare #+sbcl(sb-ext:muffle-conditions warning))
                          (iter (repeat 1) (sum 'not-a-number))))))

(def test test/basic/count ()
  (is (= 10 (eval '(iter (repeat 9.5) (count 1)))))
  (is (= 9  (eval '(iter (repeat 9) (count t)))))
  (is (= 0  (eval '(iter (repeat -1.5) (counting t))))))

(def test test/basic/initially ()
  (is (equal '(42)
             (eval '(let ((x 10))
                      (iter (initially (setf x 42))
                            (repeat 1)
                            (collect x)))))))

(def test test/basic/finally ()
  (is (= 42 (eval '(iter named alma
                    (repeat 1)
                    (finally (return-from alma 42)))))))

(def test test/basic/first-time? ()
  (is (equal '("x" "," "x" "," "x")
             (eval '(iter (repeat 3)
                          (unless (first-time?)
                            (collect ","))
                          (collect "x")))))
  (is (equal '((    "x" "," "x")
               ("," "x" "," "x"))
             (eval '(iter named outer
                          (repeat 2)
                          (collect (iter (repeat 2)
                                         (unless (first-time? :in outer)
                                           (collect ","))
                                         (collect "x"))))))))

(def test test/basic/for/in-list ()
  (is (equal '(a x b y)
             (eval '(iter (for i :in-list '(a b c))
                          (for j :in-list '(x y))
                          (collect i)
                          (collect j)))))
  (is (equal '(1 2)
             (eval '(iter outer
                     (iter (collect (for x :in-list '(1 2)) :in outer))))))
  (is (equal '((1 2 3) (2 3 4))
             (eval '(let ((data (list 1 2 3)))
                     (list (iter (for x :in-list data :mutable t)
                                 (collect x)
                                 (incf x))
                            data))))))

(def test test/basic/for/in-vector ()
  (is (equalp '(a x b y)
              (eval '(iter (for i :in-vector #(a b c))
                           (for j :in-vector #(x y))
                           (collect i)
                           (collect j)))))
  (is (equalp '(1 2)
              (eval '(iter outer
                           (iter (collect (for x :in-vector #(1 2)) :in outer))))))
  (is (equalp '((1 2 3) #(2 3 4))
              (eval '(let ((data (vector 1 2 3)))
                       (list (iter (for x :in-vector data :mutable t)
                                   (collect x)
                                   (incf x))
                             data))))))
