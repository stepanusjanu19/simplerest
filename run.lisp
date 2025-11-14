;;;; run.lisp

(load "quicklisp/setup.lisp")
(ql:quickload '(:hunchentoot :cl-json))
(load "rest.lisp")

(in-package :my-rest-server)

(defun main ()
  "Main entry point for Podman/Docker build."
  (format t "🚀 Starting Lisp API container on port ~A...~%" *port*)
  (start-server)
  (sleep most-positive-fixnum))
