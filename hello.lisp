#!sbcl --script
(write-line "hello,world")
(defun fun1 (y)
  (+ y 3))

(print (fun1 10))

(defun test-var ()
  (let ((x 10))
    (print x)
    (format t "x: ~a~%" x)))

(test-var)
(defvar *e* 12)
(print ((lambda () (format t "~a" *e*) (+ *e* 29))))
(defvar *base-path* "/home/dragon/")
(write-line *base-path*)
(write-line (concatenate 'string *base-path* "workspace")) 
(setq *base-path* (concatenate 'string *base-path* "workspace/sbcl"))
(print *base-path*)