ALTER TABLE public.units ADD CONSTRAINT units_company_fk FOREIGN KEY (company) REFERENCES public.companies (id);
ALTER TABLE public.units ADD CONSTRAINT units_type_fk FOREIGN KEY (type) REFERENCES meta.types (id);
