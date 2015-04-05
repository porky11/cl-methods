(defpackage #:cl-methods
  (:use #:common-lisp)
  (:export #:defmethod*))

(in-package :cl-methods)

(defun insert (element list)
  (if list
      (cons (cons element list)
            (mapcar (lambda (rest) (cons (car list) rest))
                    (insert element (cdr list))))
      (list (list element))))

(defun all-permutations (list)
  (if list
      (destructuring-bind (car . cdr) list
        (if cdr
            (apply #'append (mapcar (lambda (list)
                                      (insert car list))
                                    (all-permutations cdr)))
            (list list)))))


(defun different-permutations (list &key (key #'identity) (test #'eql))
    (remove-if-not (let ((specializers '()))
                 (lambda (element) (let ((specializer (mapcar key element)))
                                     (unless (member specializer specializers :test test)
                                       (push specializer specializers)))))
               (all-permutations list) :from-end t))

(defun arguments (lambda-list &optional keyword)
  (if keyword (setq lambda-list (cdr (member keyword lambda-list))))
  (loop for element in lambda-list
       while (not (member element lambda-list-keywords))
       collect element))

(defun bindings (lambda-list)
  (mapcar (lambda (element)
            (if (consp element)
                (if (consp (car element))
                    (cadar element)
                    (car element))
                element))
          lambda-list))

(defun remove-list (items &rest args)
  (apply #'remove-if (lambda (element) (member element items))
         args))

(defmacro defmethod* (name &rest args
                      &aux
                        (fname (gensym "FNAME"))
                        qualifiers
                        lambda-list
                        required
                        optional)
  (loop (let ((element (car args)))
          (if (listp element)
              (return)
              (progn
                (push element qualifiers)
                (pop args)))))
  (setq lambda-list (pop args))
  (setq optional lambda-list)
  (loop (let ((element (car optional)))
          (if (or (member element lambda-list-keywords) (null optional))
              (return)
              (progn
                (push element required)
                (pop optional)))))
  (setq required (mapcar (lambda (element)
                           (if (consp element)
                               element
                               (list element t)))
                         required))
  (setq lambda-list (remove-list lambda-list-keywords (bindings lambda-list)))
  `(progn
     (defun ,fname ,lambda-list ,@args)
     ,@(loop for permutation in (different-permutations required :key #'cadr :test #'equal)
          collect `(defmethod ,name ,@(reverse qualifiers) (,@permutation ,@optional)
                             (,fname ,@lambda-list)))))
