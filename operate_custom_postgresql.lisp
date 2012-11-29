(defvar *lisp-dirs* "~/myfile/project/stock/" "project root directory")
;(defvar *lisp-dirs* (native-namestring *default-pathname-defaults*) "project root directory")
;(format t "~a" (native-namestring *default-pathname-defaults*))
(write-line *lisp-dirs*)
(require 'asdf)
(setf asdf:*central-registry* 
      '(*default-pathname-defaults*
	(concatenate 'string *lisp-dirs* "uffi/")
	(concatenate 'string *lisp-dirs* "rt/")
	(concatenate 'string *lisp-dirs* "clsql/")))

(asdf:operate 'asdf:load-op :clsql)
(use-package :clsql-user)
;
;;(connect `("localhost" "dns_config" "root" "123123") :database-type :mysql)
(connect `("localhost" "lisp_test" "dragons_sql" "123123") :database-type :postgresql)
(start-sql-recording)
(enable-sql-reader-syntax)

(execute-command "create table if not exists haeder(id int primary key,
                                      customer varchar(50),
                                      comments varchar(50))")

;(execute-command "insert into `header`(customer,comments) values('long', 'hong')")
