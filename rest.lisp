;;;; ============================================================
;;;; Simple REST Backend Example using Hunchentoot (Modern Version)
;;;; ============================================================

(defpackage :my-rest-server
  (:use :cl :hunchentoot :cl-json)
  (:export :start-server :stop-server))
(in-package :my-rest-server)

(defparameter *port* 7000)
(defparameter *server* nil)

;; ------------------------------------------------------------
;; Utility: JSON response helper
;; ------------------------------------------------------------
(defun respond-json (data &key (status 200))
  "Return a JSON response with correct headers."
  (setf (content-type*) "application/json")
  (setf (return-code*) status)
  (encode-json-to-string data))

;; ------------------------------------------------------------
;; Example in-memory data
;; ------------------------------------------------------------
(defparameter *users*
  (list
    (list :id 1 :name "Alice" :role "admin")
    (list :id 2 :name "Bob"   :role "user")))

;; ------------------------------------------------------------
;; REST Handlers
;; ------------------------------------------------------------

;; Root route
(define-easy-handler (home :uri "/") ()
  (respond-json '(:message "Welcome to the Lisp REST API!")))

;; /users endpoint handles both GET and POST
(define-easy-handler (users :uri "/users") ()
  (let ((method (request-method*)))
    (ecase method
      (:get
       ;; Handle GET /users
       (respond-json (list :users *users*)))
      (:post
       ;; Handle POST /users
       (let* ((json (decode-json-from-string (raw-post-data :force-text t)))
              (name (getf json :name))
              (role (getf json :role))
              (new-id (1+ (apply #'max (mapcar #'(lambda (u) (getf u :id)) *users*)))))
         (push (list :id new-id :name name :role role) *users*)
         (respond-json `(:message "User created" :id ,new-id))))
      (t
       (respond-json '(:error "Method not allowed") :status 405)))))

;; /user?id=1 (use query param instead of path param)
(define-easy-handler (user :uri "/user") (id)
  (let* ((uid (parse-integer id :junk-allowed t))
         (user (find uid *users* :key #'(lambda (u) (getf u :id)))))
    (if user
        (respond-json user)
        (respond-json '(:error "User not found") :status 404))))

;; ------------------------------------------------------------
;; Start / Stop functions
;; ------------------------------------------------------------
(defun start-server ()
  (unless *server*
    (setf *server* (start (make-instance 'easy-acceptor :port *port*))))
  (format t "~%✅ Server running on http://localhost:~A/~%" *port*))

(defun stop-server ()
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "~%🛑 Server stopped.~%")))

