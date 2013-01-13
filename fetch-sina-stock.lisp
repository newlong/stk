(defvar *lisp-dirs* "~/myfile/project/stock/" "project root directory")
;;;测试抓取网页信息
(ql:quickload :drakma)
(setf drakma:*header-stream* *standard-output*)

;(require 'asdf)
;(setf asdf:*central-registry*
;	  '(*default-pathname-defaults*
;		(concatenate 'string *lisp-dirs* "cl-html-parse/")))
;(asdf:operate 'asdf:load-op :cl-html-parse)

(ql:quickload :closure-html)
(asdf:operate 'asdf:load-op :cxml-stp)

(defparameter *stock-index-content* (drakma:http-request "http://finance.sina.com.cn/stock/"))
(defparameter *stock-content-parse* (chtml:parse *stock-index-content* (cxml-stp:make-builder)))

(defun print-stock-category ()
  (stp:do-recursively(div *stock-content-parse*)
	(when (and (typep div 'stp:element)
			   (equal (stp:local-name div) "div")
			   (equal (stp:attribute-value div "class") "nav_int"))
	  (format t "~a" div))))