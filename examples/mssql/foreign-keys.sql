ALTER TABLE PUBLIC.UNITS ADD CONSTRAINT FK_UNITS_COMPANY FOREIGN KEY (COMPANY) REFERENCES PUBLIC.COMPANIES (ID)
GO
ALTER TABLE PUBLIC.UNITS ADD CONSTRAINT FK_UNITS_TYPE FOREIGN KEY (TYPE) REFERENCES META.TYPES (ID)
GO