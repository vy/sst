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


;;; Special Variable Definitions

(def (class enc) rdbms-pgsql ()
  ((name :type string :initform "PostgreSQL")))


;;; Data Type Parsing Methods

(define-data-type-parser (rdbms-pgsql bigint attrs)
  "bigint")

(define-data-type-parser (rdbms-pgsql bigserial attrs)
  "bigserial")

(define-data-type-parser (rdbms-pgsql bit attrs)
  (format-string "bit~@[(~d)~]" (car attrs)))

(define-data-type-parser (rdbms-pgsql char attrs)
  (format-string "char~@[(~d)~]" (car attrs)))

(define-data-type-parser (rdbms-pgsql float attrs)
  "float")

(define-data-type-parser (rdbms-pgsql int attrs)
  "int")

(define-data-type-parser (rdbms-pgsql serial attrs)
  "serial")

(define-data-type-parser (rdbms-pgsql smallint attrs)
  "smallint")

(define-data-type-parser (rdbms-pgsql text attrs)
  "text")

(define-data-type-parser (rdbms-pgsql timestamp attrs)
  (format-string "timestamp with~:[out~;~] time zone"
                 (getf attrs :with-time-zone)))

(define-data-type-parser (rdbms-pgsql varchar attrs)
  (format-string "~:[text~;varchar(~:*~d)~]" (car attrs)))


;;; Identifier Formatting Methods

(def method %format-primary-key-identifier
    ((rdbms rdbms-pgsql) table-name column-name)
  (format-string "~a_~a_pk" table-name column-name))

(def method %format-foreign-key-identifier
    ((rdbms rdbms-pgsql) table-name column-name)
  (format-string "~a_~a_fk" table-name column-name))

(def method %format-index-identifier
    ((rdbms rdbms-pgsql) table-name column-names)
  (format-string
   "~a_~{~{~a~:[~;_desc~]~}~^_~}_idx"
   table-name
   (mapcar (lambda (column-name)
             (list (car column-name)
                   (getf (cdr column-name) :descending)))
           column-names)))

(def method %format-unique-index-identifier
    ((rdbms rdbms-pgsql) table-name column-names)
  (format-string
   "~a_~{~{~a~:[~;_desc~]~}~^_~}_unq"
   table-name
   (mapcar (lambda (column-name)
             (list (car column-name)
                   (getf (cdr column-name) :descending)))
           column-names)))


;;; Rendering Methods

(def method %create-schema ((rdbms rdbms-pgsql) schema)
  (format-stream "CREATE SCHEMA ~a;~%" (icc (name-of schema))))

(def method %create-table ((rdbms rdbms-pgsql) schema table)
  (format-stream
   (string-append
    "CREATE TABLE ~a.~a (~%"
    "~{~&~{    ~a ~a~:[ NOT NULL~;~]~@[ DEFAULT ~a~]~}~^,~}"
    "~&);~%")
   (icc (name-of schema))
   (icc (name-of table))
   (mapcar
    (lambda (column)
      (list (icc (name-of column))
            (data-type-name-of column)
            (nullable-p-of column)
            (default-value-of column)))
    (columns-of table)))
  ;; If any :START or :INCREMENT parameters supplied with columns of data type
  ;; (big)serial, we'll need to alter related sequences.
  (dolist (column
            (sort
             (remove-if-not
              (lambda (data-type)
                (and (member (car data-type) (list 'serial 'bigserial))
                     (or (find :start (cdr data-type))
                         (find :increment (cdr data-type)))))
              (columns-of table)
              :key #'data-type-of)
             #'string< :key #'name-of))
    (format-stream
     (string-append
      "ALTER SEQUENCE ~a.~a_~a_seq"
      "~@[ START ~d~]"
      "~@[ INCREMENT ~d~];~%")
     (icc (name-of schema))
     (icc (name-of table))
     (icc (name-of column))
     (getf (cdr (data-type-of column)) :start)
     (getf (cdr (data-type-of column)) :increment))))

(def method %create-primary-key ((rdbms rdbms-pgsql) schema table pk-constraint)
  (let ((table-name (name-of table))
        (column-name (column-name-of pk-constraint)))
    (format-stream "ALTER TABLE ~a.~a ADD CONSTRAINT ~a PRIMARY KEY (~a);~%"
                   (icc (name-of schema))
                   (icc table-name)
                   (icc (format-primary-key-identifier table-name column-name))
                   (icc column-name))))

(def method %create-foreign-key ((rdbms rdbms-pgsql) schema table fk-constraint)
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
      ";~%")
     (icc (name-of schema))
     (icc table-name)
     (icc (format-foreign-key-identifier table-name column-name))
     (icc column-name)
     (icc (ref-schema-name-of fk-constraint))
     (icc (ref-table-name-of fk-constraint))
     (icc (ref-column-name-of fk-constraint))
     (on-delete-cascade-p-of fk-constraint)
     (on-update-cascade-p-of fk-constraint))))

(def method %create-index ((rdbms rdbms-pgsql) schema table index)
  (let ((table-name (name-of table))
        (column-names (column-names-of index))
        (unique-p (unique-p-of index)))
    (format-stream
     "CREATE~:[~; UNIQUE~] INDEX ~a ON ~a.~a (~{~{~a~:[~; DESC~]~}~^, ~});~%"
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
