;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate.test)

(def suite* (test/nesting :in test))

(def test test/nesting/1 ()
  #+nil ;; TODO expands into infinite loop
  (with-expected-failures
    (is (equal '(a b c)
               (eval '(iter named outer
                       (for i :in-list '(a b c))
                       (macrolet ((wrapper (&body body)
                                    ;; FIXME unfortunately we can't detect this ITER due to ` hiding it from us
                                    `(iter named inner
                                           (repeat 2)
                                           ,@body)))
                         (wrapper
                          ;; this collecting should collect into OUTER
                          (collecting i)))))))))

(def test test/nesting/2 ()
  (is (equal '(a a b b c c)
             (eval '(macrolet ((wrapper (&body body)
                                `(iter named inner
                                       (repeat 2)
                                       ,@body)))
                     (iter named outer
                           (for i :in-list '(a b c))
                           (wrapper
                            ;; this collecting should collect into OUTER
                            (collecting i))))))))

(def test test/nesting/3 ()
  (is (equal '(a a a b b b c c c)
             (eval '(iter outer
                          (for i :in-list '(a b c))
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
                            (collecting i)
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
