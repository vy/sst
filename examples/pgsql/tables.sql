CREATE TABLE meta.types (
    id serial,
    name text
);
CREATE TABLE public.companies (
    id serial,
    name text NOT NULL
);
CREATE TABLE public.units (
    id serial,
    type char,
    code float DEFAULT -3.4,
    company int
);
ALTER SEQUENCE public.units_id_seq START 10 INCREMENT 2;
