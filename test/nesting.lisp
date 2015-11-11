;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/nesting :in test))

(def test test/nesting/1 ()
  (with-expected-failures
    (is (equal '(a a b b c c)
               (eval '(iter named outer
                       (for i :in-list '(a b c))
                       (macrolet ((wrapper (&body body)
                                    ;; FIXME unfortunately we can't detect this ITER due to ` hiding it from us while processing OUTER
                                    ;; maybe support Fare's transparent quasi-quote so that we can look inside?
                                    `(iter named inner
                                           (repeat 2) ; and due to that this repeat is processed in OUTER
                                           ,@body)))
                         (wrapper
                          ;; this collecting should collect into OUTER (due to clauses being lexically scoped)
                          (collecting i)))))))))

(def test test/nesting/lexical-scoping-of-clauses ()
  (is (equal '(a a b b c c)
             (eval '(macrolet ((wrapper (&body body)
                                `(iter named inner
                                       (repeat 2)
                                       ,@body)))
                     (iter named outer
                           (for i :in-list '(a b c))
                           (wrapper
                            ;; this collecting should collect into OUTER (due to clauses being lexically scoped)
                            (collecting i)))))))
  (is (equal '((1 2 3) (1 2 3))
             (eval '(iter named outer
                          (repeat 2)
                          (collecting (iter (for i :from 1 :to 10)
                                            (collecting i)
                                            (when (> i 2)
                                              (block nil
                                                ;; make sure FINISH does not rely on the name of the ITER and is lexically scoped to the inner loop
                                                (finish)))))))))
  (is (equal '(1 2 3)
             (eval '(iter named outer
                          (repeat 2)
                          (iter named inner
                                (for i :from 1 :to 10000)
                                (collecting i :in outer)
                                (when (> i 2)
                                  (finish :in outer))))))))

(def test test/nesting/3 ()
  (is (equal '(a a a b b b c c c)
             (eval '(iter outer
                          (for i :in-list '(a b c))
                          ;; may break due to the wrapping PROGN
                          (progn
                            (iter inner
                                  (repeat 3)
                                  (collecting i :in outer))))))))

(def test test/nesting/4 ()
  (is (equal '(A A 1 2 3 A 1 2 3 B B 1 2 3 B 1 2 3 C C 1 2 3 C 1 2 3)
             (eval '(iter level1
                          (for i :in-list '(a b c))
                          (progn
                            (collecting i)
                            (iter level2
                                  (repeat 2)
                                  (collecting i :in level1)
                                  (iter level3
                                        (for i :in-list '(1 2 3))
                                        (collecting i :in level1)))))))))

(def test test/nesting/next-iteration ()
  (is (equal '(A A 1 2 A 1 2 B B 1 2 B 1 2 C C 1 2 C 1 2)
             (eval '(iter level1
                          (for i :in-list '(a b c))
                          (progn
                            (collecting i) ; collects in level1 (lexical)
                            (iter level2
                                  (repeat 2)
                                  (collecting i :in level1)
                                  (iter level3
                                        (for i :in-list '(1 2 3))
                                        (collecting i :in level1)
                                        (when (evenp i)
                                          (next-iteration :in level2))))))))))

(def test test/nesting/5 ()
  (is (equal '(a 42 b 42 c 42)
             (eval '(iter named outer
                          (for i :in-list '(a b c))
                          (macrolet ((wrapper (&body body)
                                       `(progn
                                          (collecting i)
                                          ,@body)))
                            (wrapper
                             (collecting 42))))))))

(def test test/nesting/compex-nested-collecting ()
  (is (equal '(1 1 100 200 300 1 100 200 300 (2 (101 201 301) 2 (101 201 301))
               2 2 100 200 300 2 100 200 300 (3 (101 201 301) 3 (101 201 301))
               3 3 100 200 300 3 100 200 300 (4 (101 201 301) 4 (101 201 301)))
             (eval '(iter level1
                          (for i :in-list '(1 2 3))
                          (progn
                            (collecting i)
                            (collecting (iter level2
                                              (repeat 2)
                                              (collecting i :in level1)
                                              (collecting (1+ i))
                                              (collecting (iter level3
                                                                (for i :in-list '(100 200 300))
                                                                (collecting i :in level1)
                                                                (collecting (1+ i))))))))))))

(def test test/nesting/collecting-from-macro ()
  (is (equal '(42 (101 101) 42 (101 101))
             (eval '(macrolet ((inject (&body body)
                                `(collecting (progn ,@body))))
                     (iter named outer
                           (repeat 2)
                           (inject 42)
                           (collecting (iter named inner
                                             (repeat 2)
                                             (inject 101)))))))))
