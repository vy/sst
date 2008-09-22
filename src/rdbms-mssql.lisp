;;; Copyright (c) 2008, Volkan YAZICI <volkan.yazici@gmail.com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:

;;; - Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.

;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;   this list of conditions and the following disclaimer in the documentation
;;;   and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :sst)


;;; Special Variables

(def (class enc) rdbms-mssql ()
  ((name :type string :initform "Microsoft SQL Server")))


;;; Data Type Parsing Methods

(define-data-type-parser (rdbms-mssql bigint attrs)
  "bigint")

(define-data-type-parser (rdbms-mssql bigserial attrs)
  (format-string "bigint IDENTITY(~d,~d)"
                 (getf attrs :start 1)
                 (getf attrs :increment 1)))

(define-data-type-parser (rdbms-mssql bit attrs)
  (format-string "bit~@[(~d)~]" (car attrs)))

(define-data-type-parser (rdbms-mssql char attrs)
  (format-string "~:[~;n~]char~@[(~a)~]"
                 (getf (if (keywordp (car attrs)) attrs (cdr attrs))
                       :unicode)
                 (and (numberp (car attrs)) (car attrs))))

(define-data-type-parser (rdbms-mssql float attrs)
  "float")

(define-data-type-parser (rdbms-mssql int attrs)
  "int")

(define-data-type-parser (rdbms-mssql serial attrs)
  (format-string "int IDENTITY(~d,~d)"
                 (getf attrs :start 1)
                 (getf attrs :increment 1)))

(define-data-type-parser (rdbms-mssql smallint attrs)
  "smallint")

(define-data-type-parser (rdbms-mssql text attrs)
  (format-string "~:[~;n~]varchar(max)" (getf attrs :unicode)))

(define-data-type-parser (rdbms-mssql timestamp attrs)
  "datetime")

(define-data-type-parser (rdbms-mssql varchar attrs)
  (format-string "~:[~;n~]varchar(~:[max~;~:*~d~])"
                 (getf (if (keywordp (car attrs)) attrs (cdr attrs)) :unicode)
                 (when (numberp (car attrs)) (car attrs))))


;;; Formatting Methods

(def method %format-primary-key-identifier
    ((rdbms rdbms-mssql) table-name column-name)
  (format-string "pk_~a_~a" table-name column-name))

(def method %format-foreign-key-identifier
    ((rdbms rdbms-mssql) table-name column-name)
  (format-string "fk_~a_~a" table-name column-name))

(def method %format-index-identifier
    ((rdbms rdbms-mssql) table-name column-names)
  (format-string
   "ix_~a_~{~{~a~:[~;_desc~]~}~^_~}"
   table-name
   (mapcar (lambda (column-name)
             (list (car column-name)
                   (getf (cdr column-name) :descending)))
           column-names)))

(def method %format-unique-index-identifier
    ((rdbms rdbms-mssql) table-name column-names)
  (format-string
   "uk_~a_~{~{~a~:[~;_desc~]~}~^_~}"
   table-name
   (mapcar (lambda (column-name)
             (list (car column-name)
                   (getf (cdr column-name) :descending)))
           column-names)))


;;; Rendering Methods

(def method %create-schema ((rdbms rdbms-mssql) schema)
  (format-stream "CREATE SCHEMA ~a~%GO~%" (icc (name-of schema))))

(def method %create-table ((rdbms rdbms-mssql) schema table)
  (format-stream
   (string-append
    "CREATE TABLE ~a.~a (~%"
    "~{~&~{    ~a ~a~:[ NOT NULL~;~]~@[ DEFAULT ~a~]~}~^,~}"
    "~&)~%GO~%")
   (icc (name-of schema))
   (icc (name-of table))
   (mapcar
    (lambda (column)
      (list (icc (name-of column))
            (data-type-name-of column)
            (nullable-p-of column)
            (default-value-of column)))
    (columns-of table))))

(def method %create-primary-key ((rdbms rdbms-mssql) schema table pk-constraint)
  (let ((table-name (name-of table))
        (column-name (column-name-of pk-constraint)))
    (format-stream "ALTER TABLE ~a.~a ADD CONSTRAINT ~a PRIMARY KEY (~a)~%GO~%"
                   (icc (name-of schema))
                   (icc table-name)
                   (icc (format-primary-key-identifier table-name column-name))
                   (icc column-name))))

(def method %create-foreign-key ((rdbms rdbms-mssql) schema table fk-constraint)
  (let ((table-name (name-of table))
        (column-name (column-name-of fk-constraint)))
    (format-stream
     (string-append
      "ALTER TABLE ~a.~a "
      "ADD CONSTRAINT ~a "
      "FOREIGN KEY (~a) "
      "REFERENCES ~a.~a (~a)"
      "~:[~; ON DELETE CASCADE~]"
      "~:[~; ON UPDATE CASCADE~]"
      "~%GO~%")
     (icc (name-of schema))
     (icc table-name)
     (icc (format-foreign-key-identifier table-name column-name))
     (icc column-name)
     (icc (ref-schema-name-of fk-constraint))
     (icc (ref-table-name-of fk-constraint))
     (icc (ref-column-name-of fk-constraint))
     (on-delete-cascade-p-of fk-constraint)
     (on-update-cascade-p-of fk-constraint))))

(def method %create-index ((rdbms rdbms-mssql) schema table index)
  (let ((table-name (name-of table))
        (column-names (column-names-of index))
        (unique-p (unique-p-of index)))
    (format-stream
     "CREATE~:[~; UNIQUE~] INDEX ~a ON ~a.~a (~{~{~a~:[~; DESC~]~}~^, ~})~%GO~%"
     unique-p
     (icc
      (if unique-p
          (format-unique-index-identifier table-name column-names)
          (format-index-identifier table-name column-names)))
     (icc (name-of schema))
     (icc table-name)
     (mapcar (lambda (column-name)
               (list (icc (car column-name))
                     (getf (cdr column-name) :descending)))
             column-names))))