
;;;; ============================================================
;;;; run.lisp — Entry point for Docker/Podman Lisp API
;;;; ============================================================

(load "quicklisp/setup.lisp")
(ql:quickload '(:hunchentoot :cl-json))
(load "rest.lisp")

(defun main ()
  "Main entry point for Podman/Docker build."
  (format t "🚀 Starting Lisp API container on port 7000...~%")
  (my-rest-server:start-server)
  ;; Prevent SBCL from exiting
  (sleep most-positive-fixnum))
