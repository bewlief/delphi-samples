

CREATE TABLE country
(
  country character varying(15) NOT NULL,
  currency character varying(10) NOT NULL,
  CONSTRAINT country_pkey PRIMARY KEY (country )
);

INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('USA', 'Dollar');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('England', 'Pound');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Canada', 'CdnDlr');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Switzerland', 'SFranc');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Japan', 'Yen');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Italy', 'Lira');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('France', 'FFranc');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Germany', 'D-Mark');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Australia', 'ADollar');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Hong Kong', 'HKDollar');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Netherlands', 'Guilder');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Belgium', 'BFranc');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Austria', 'Schilling');
INSERT INTO COUNTRY (COUNTRY, CURRENCY) VALUES ('Fiji', 'FDollar');
