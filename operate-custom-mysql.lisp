;(defvar *lisp-dirs* "~/workspace/stock/" "project root directory")
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
(connect `("127.0.0.1" "dns_config" "root" "123123") :database-type :mysql)
;(start-sql-recording)
;(enable-sql-reader-syntax)

;(execute-command "create table if not exists `haeder`(`id` int primary key,
;                                      `customer` varchar(50),
;                                      `comments` varchar(50))")

<<<<<<< Updated upstream
(execute-command "create table if not exists `header`(`id` int primary key auto_increment,
                                      `customer` varchar(50),
                                      `comments` varchar(50))")

;; insert some record to mysql
(execute-command "insert into `header`(`customer`,`comments`)
                               values('newlong', 'my name is dragon')")
=======
;(execute-command "insert into `header`(customer,comments) values('long', 'hong')")
>>>>>>> Stashed changes
