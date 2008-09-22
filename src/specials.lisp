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


;;; SQL Object Classes

(def (class nc) sql-schema ()
  ((name :type string)
   (tables)))

(def (class nc) sql-table ()
  ((name :type string)
   (columns)
   (constraints)
   (indexes)))

(def (class nc) sql-column ()
  ((name :type string)
   (data-type)
   (data-type-name :type string)
   (nullable-p :type boolean)
   (default-value)))

(def (class nc) sql-primary-key-constraint ()
  ((column-name :type string)))

(def (class nc) sql-foreign-key-constraint ()
  ((column-name :type string)
   (ref-schema-name :type string)
   (ref-table-name :type string)
   (ref-column-name :type string)
   (on-delete-cascade-p :type boolean)
   (on-update-cascade-p :type boolean)))

(def (class nc) sql-index ()
  ((column-names)
   (unique-p :type boolean)))


;;; SQL Object Accumulators

(def special-variable *sql-schemas*)

(def special-variable *sql-tables*)

(def special-variable *sql-columns*)

(def special-variable *sql-constraints*)

(def special-variable *sql-indexes*)


;;; Parsing State Variables

(def special-variable *current-rdbms*)

(def special-variable *current-stream*)

(def special-variable *current-schema*)

(def special-variable *current-table*)

(def special-variable *current-column*)

(def special-variable *current-identifier-case*)


;;; RDBMS Specific Parsing Generics

(def generic parse-data-type (rdbms data-type &rest attrs))

(def macro define-data-type-parser ((rdbms data-type attrs) &body body)
  `(def method parse-data-type
       ((rdbms ,rdbms) (data-type (eql ',data-type)) &rest ,attrs)
     (declare (ignorable ,attrs))
     ,@body))


;;; RDBMS Specific Rendering Generics

(def macro make-rdbms-macro (&rest specs)
  `(progn
     ,@(reduce
        (lambda (accum spec)
          (destructuring-bind (macro-sym &rest args) spec
            (let ((method-sym
                   (intern
                    (concatenate
                     'string "%" (symbol-name macro-sym)))))
              (append
               `((def generic ,method-sym ,args)
                      (def macro ,macro-sym ,(rest args)
                        `(,',method-sym *current-rdbms* ,,@(rest args))))
               accum))))
        specs :initial-value nil)))

(make-rdbms-macro
 (format-primary-key-identifier rdbms table-name column-name)
 (format-foreign-key-identifier rdbms table-name column-name)
 (format-index-identifier rdbms table-name column-names)
 (format-unique-index-identifier rdbms table-name column-names))

(make-rdbms-macro
 (create-schema rdbms schema)
 (create-table rdbms schema table)
 (create-primary-key rdbms schema table pk-constraint)
 (create-foreign-key rdbms schema table fk-constraint)
 (create-index rdbms schema table index))
