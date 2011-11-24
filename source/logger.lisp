;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.reiterate)

#*(((find-package :hu.dwim.logger)
    ;; if we are loaded after :hu.dwim.logger, then use a full-featured logger
    (def hu.dwim.logger:logger log ()))

   (t
    ;; otherwise only a minimalistic hu.dwim.logger simulation
    (macrolet ((frob (name)
                `(def macro ,name (message &rest args)
                   (declare (ignore message args))
                   ;; `(format *debug-io* ,(string+ message "~%") ,@args)
                   `(values))))
     (frob log.info)
     (frob log.debug)
     (frob log.dribble))))
