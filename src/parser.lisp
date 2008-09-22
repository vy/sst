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


;;; Main Parsing Routines

(def macro with-schema (attrs name &body components)
  (declare (ignore attrs))
  `(let ((*current-schema* ,name)
         (*sql-tables*))
     ,@components
     (push
      (make-instance
       'sql-schema
       :name *current-schema*
       :tables *sql-tables*)
      *sql-schemas*)))

(def macro with-table (attrs name columns &body components)
  (declare (ignore attrs))
  `(let ((*current-table* ,name)
         (*sql-columns*)
         (*sql-constraints*)
         (*sql-indexes*))
     ,@(mapcar (lambda (column-form) `(with-column ,@column-form)) columns)
     ,@components
     (push
      (make-instance
       'sql-table
       :name *current-table*
       :columns (reverse *sql-columns*)
       :constraints *sql-constraints*
       :indexes *sql-indexes*)
      *sql-tables*)))

(def macro with-column (name data-type &rest attrs)
  `(let ((*current-column* ,name))
     ,@(when (getf attrs :primary-key) `((with-primary-key)))
     ,@(when-let (attr (getf attrs :foreign-key))
         `((with-foreign-key ,@(ensure-list attr))))
     (push
      (make-instance
       'sql-column
       :name *current-column*
       :data-type ',(ensure-list data-type)
       :data-type-name (with-data-type ,@(ensure-list data-type))
       :nullable-p ,(not (getf attrs :not-null))
       :default-value ,(getf attrs :default))
      *sql-columns*)))

(def macro with-primary-key ()
  `(push
    (make-instance
     'sql-primary-key-constraint
     :column-name *current-column*)
    *sql-constraints*))

(def macro with-foreign-key
    (table &key schema on-delete-cascade on-update-cascade)
  `(push
    (make-instance
     'sql-foreign-key-constraint
     :column-name *current-column*
     :ref-schema-name (or ,schema *current-schema*)
     :ref-table-name (or ,table *current-table*)
     :on-delete-cascade-p ,on-delete-cascade
     :on-update-cascade-p ,on-update-cascade)
    *sql-constraints*))

(def macro with-data-type (data-type &rest attrs)
  `(parse-data-type *current-rdbms* ',data-type ,@attrs))

(def macro with-index (attrs &rest columns)
  `(push
    (make-instance
     'sql-index
     :column-names ',(mapcar #'ensure-list columns)
     :unique-p ,(getf attrs :unique))
    *sql-indexes*))


;;; Main Rendering Routines

(defmacro render-schemas ()
  `(dolist (schema *sql-schemas*)
     (create-schema schema)))

(defmacro render-tables ()
  `(dolist (schema *sql-schemas*)
     (dolist (table (tables-of schema))
       (create-table schema table))))

(defmacro render-primary-keys ()
  `(dolist (schema *sql-schemas*)
     (dolist (table (tables-of schema))
       (when-let (pk-constraint
                  (find
                   'sql-primary-key-constraint
                   (constraints-of table)
                   :key #'type-of))
         (create-primary-key schema table pk-constraint)))))

(defmacro render-foreign-keys ()
  `(dolist (schema *sql-schemas*)
     (dolist (table (tables-of schema))
       (dolist (fk-constraint
                 (sort
                  (remove-if-not
                   (lambda (constraint)
                     (typep constraint 'sql-foreign-key-constraint))
                   (constraints-of table))
                  #'string<
                  :key (lambda (constraint)
                         (format-string
                          "~{~a~^ ~}"
                          (list
                           (column-name-of constraint)
                           (ref-schema-name-of constraint)
                           (ref-table-name-of constraint)
                           (ref-column-name-of constraint))))))
         (create-foreign-key schema table fk-constraint)))))

(defmacro render-indexes ()
  `(dolist (schema *sql-schemas*)
     (dolist (table (tables-of schema))
       (dolist (index
                 (sort
                  (copy-list (indexes-of table))
                  #'string<
                  :key (lambda (index)
                         (format-string
                          "~{~a~^ ~}"
                          (mapcar #'car (column-names-of index))))))
         (create-index schema table index)))))


;;; Main Producer Routines

(def macro init-foreign-key-slots ()
  `(dolist (schema *sql-schemas*)
     (dolist (table (tables-of schema))
       (dolist (fk-constraint
                 ;; Sort constraints according to concatenated name order.
                 (sort
                  ;; Extract SQL-FOREIGN-KEY-CONSTRAINTs.
                  (remove-if-not
                   (lambda (constraint)
                     (typep constraint 'sql-foreign-key-constraint))
                   (constraints-of table))
                  #'string<
                  :key (lambda (constraint)
                         (format-string
                          "~a ~a"
                          (ref-schema-name-of constraint)
                          (ref-table-name-of constraint)))))
         (let* ((ref-table
                 (or (find
                      ;; Find referenced table.
                      (ref-table-name-of fk-constraint)
                      (tables-of
                       ;; Find referenced schema.
                       (or (find (ref-schema-name-of fk-constraint)
                                 *sql-schemas*
                                 :key #'name-of
                                 :test #'string=)
                           (error (string-append
                                   "Couldn't find referenced schema "
                                   "for constraint ~a.")
                                  fk-constraint)))
                      :key #'name-of
                      :test #'string=)
                     (error
                      "Couldn't find referenced table for constraint ~a."
                      fk-constraint)))
                (ref-column-name
                 (column-name-of
                  ;; Find a SQL-PRIMARY-KEY-CONSTRAINT on the referenced table.
                  (or (find 'sql-primary-key-constraint
                            (constraints-of ref-table)
                            :key #'type-of)
                      (error (string-append
                              "Couldn't find a primary key on referenced "
                              "table for constraint ~a.")
                             fk-constraint)))))
           ;; Check data-type consistency!
           (let ((column
                  (find (column-name-of fk-constraint)
                        (columns-of table)
                        :key #'name-of
                        :test #'string=))
                 (ref-column
                  (find ref-column-name
                        (columns-of ref-table)
                        :key #'name-of
                        :test #'string=)))
             (unless (data-types-compatible-p column ref-column)
               (format-warning
                (string-append
                 "Expected ~a data type doesn't match with the "
                 "referenced ~a data type for constraint ~a on "
                 "~s column of ~s table in ~s schema.~%")
                (data-type-of column) (data-type-of ref-column)
                fk-constraint (name-of column) (name-of table)
                (name-of schema))))
           ;; Check required indexes for ON DELETE CASCADE and ON UPDATE
           ;; CASCADE modifiers.
           (when (and
                  (or (on-delete-cascade-p-of fk-constraint)
                      (on-update-cascade-p-of fk-constraint))
                  (not
                   (or
                    ;; This column should be a primary key,
                    (when-let (pk-constraint
                               (find 'sql-primary-key-constraint
                                     (constraints-of table)
                                     :key #'type-of))
                      (string= (column-name-of pk-constraint)
                               (column-name-of fk-constraint)))
                    ;; or this column should have an explicit index.
                    (some
                     (lambda (index)
                       (member (column-name-of fk-constraint)
                               (mapcar #'car (column-names-of index))
                               :test #'string=))
                     (indexes-of table)))))
             (format-warning
              (string-append
               "Missing index for ON DELETE CASCADE and/or ON UPDATE CASCADE "
               "modifier(s) of constraint ~a of \"~a\" table in \"~a\" "
               "schema.~%")
              fk-constraint (name-of table) (name-of schema)))
           ;; Fill REF-COLUMN-NAME slot.
           (setf (ref-column-name-of fk-constraint) ref-column-name))))))

(def (macro e) produce-sql-output
    (pathname  output-directory rdbms &key
     (identifier-case :downcase)
     (schema-output-file "schemas.sql")
     (table-output-file "tables.sql")
     (primary-key-output-file "primary-keys.sql")
     (foreign-key-output-file "foreign-keys.sql")
     (index-output-file "indexes.sql"))
  (assert (member identifier-case (list :upcase :downcase :quote)))
  `(let ((*current-rdbms* (make-instance ,rdbms))
         (*current-stream* *standard-output*)
         (*current-identifier-case* ,identifier-case)
         (*sql-schemas*))
     ;; Read schema structure.
     ,@(with-open-file (in pathname)
         (with-standard-io-syntax
           (let ((*package* (find-package :sst)))
             (loop for form =  (read in nil nil)
                   while form collect form))))
     ;; Fill missing SQL-FOREIGN-KEY-CONSTRAINT slots and make data-type
     ;; consistency checks.
     (init-foreign-key-slots)
     ;; Sort schemas.
     (let ((*sql-schemas* (sort *sql-schemas* #'string< :key #'name-of)))
       ;; Sort tables.
       (dolist (schema *sql-schemas*)
         (setf (tables-of schema)
               (sort (tables-of schema) #'string< :key #'name-of)))
       ;; Start rendering collected schema information.
       (with-output-redirection (,output-directory ,schema-output-file)
         (render-schemas))
       (with-output-redirection (,output-directory ,table-output-file)
         (render-tables))
       (with-output-redirection (,output-directory ,primary-key-output-file)
         (render-primary-keys))
       (with-output-redirection (,output-directory ,foreign-key-output-file)
         (render-foreign-keys))
       (with-output-redirection (,output-directory ,index-output-file)
         (render-indexes)))))
