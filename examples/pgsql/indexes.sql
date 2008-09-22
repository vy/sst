CREATE INDEX units_code_desc_type_idx ON public.units (code DESC, type);
CREATE UNIQUE INDEX units_type_unq ON public.units (type);
