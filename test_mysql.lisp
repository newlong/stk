#!/usr/local/sbcl/bin/sbcl --script

(defvar *lisp-dirs* "~/workspace/stock/" "project root directory")
(require 'asdf)
(setf asdf:*central-registry* 
      '(*default-pathname-defaults*
	(concatenate 'string "~/workspace/stock/" "uffi/")
	(concatenate 'string "~/workspace/stock/" "rt/")
	(concatenate 'string *lisp-dirs* "clsql/")))



(asdf:oos 'asdf:test-op 'clsql)