;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

;; this mess here is for having fewer dependencies. if you need logging for debugging, then edit the read-time condition below and recompile.
#*((nil
    (def hu.dwim.logger:logger reiterate () :accessor-name-prefix #:log.))
   (t
    ;; otherwise only a fake minimalistic hu.dwim.logger emulation
    (macrolet ((frob (name)
                `(def macro ,name (message &rest args)
                   (declare (ignore message args))
                   ;; `(format *debug-io* ,(string+ message "~%") ,@args)
                   `(values))))
     (frob log.fatal)
     (frob log.error)
     (frob log.warn)
     (frob log.info)
     (frob log.debug)
     (frob log.dribble))))
