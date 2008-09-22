(with-schema ()
    "public"
  (with-table ()
      "companies"
      (("id" serial :primary-key t)
       ("name" text :not-null t)))
  (with-table ()
      "units"
      (("id" (serial :start 10 :increment 2) :primary-key t)
       ("type" char :foreign-key ("types" :schema "meta"))
       ("code" float :default -3.4)
       ("company" int :foreign-key "companies"))
    (with-index (:unique t) "type")
    (with-index () ("code" :descending t) "type")))

(with-schema ()
    "meta"
  (with-table ()
      "types"
      (("id" serial :primary-key t)
       ("name" text))))
