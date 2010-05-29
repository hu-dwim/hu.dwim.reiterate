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
  (with-expected-failures
    (is (equal '(1 1 2 2 3 3)
               (eval '(iter (for i :in-list '(1 2 3))
                            (collect i)
                            (collect i)))))))
