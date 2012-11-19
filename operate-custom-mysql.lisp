(defvar *lisp-dirs* "~/workspace/stock/" "project root directory")
(require 'asdf)
(setf asdf:*central-registry* 
      '(*default-pathname-defaults*
	(concatenate 'string "~/workspace/stock/" "uffi/")
	(concatenate 'string "~/workspace/stock/" "rt/")
	(concatenate 'string *lisp-dirs* "clsql/")))


(asdf:operate 'asdf:load-op :clsql)
(use-package :clsql-user)
(connect `("localhost" "dns_config" "root" "123123") :database-type :mysql)
(start-sql-recording)
(enable-sql-reader-syntax)

(execute-command "create table `haeder`(`id` int primary key,
                                      `customer` varchar(50),
                                      `comments` varchar(50))")