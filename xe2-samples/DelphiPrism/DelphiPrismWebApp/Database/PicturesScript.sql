/******************************************************************************/
/****                   Pictures.ib Databsae Script                        ****/
/******************************************************************************/

SET SQL DIALECT 3;

SET NAMES WIN1252;

CREATE DATABASE 'C:\WorldPictures.ib'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 4096
DEFAULT CHARACTER SET WIN1252;



/******************************************************************************/
/****                                Tables                                ****/
/******************************************************************************/



CREATE TABLE CITY (
    ID_CITY  INTEGER NOT NULL,
    CITY     VARCHAR(60) NOT NULL,
    STATE    CHAR(2) NOT NULL,
    COUNTRY  VARCHAR(30) NOT NULL
);


CREATE TABLE PICTURES (
    ID_PICTURE    INTEGER NOT NULL,
    ID_CITY       INTEGER NOT NULL,
    PICTURE_NAME  VARCHAR(50) NOT NULL,
    PATH          VARCHAR(150) NOT NULL
);


CREATE TABLE ROLES (
    ID_ROLE    INTEGER NOT NULL,
    ROLE_NAME  VARCHAR(30) NOT NULL
);


CREATE TABLE USER_ROLE (
    ID_USER  INTEGER NOT NULL,
    ID_ROLE  INTEGER NOT NULL
);


CREATE TABLE USERS (
    ID_USER        INTEGER NOT NULL,
    NAME           VARCHAR(30) NOT NULL,
    USER_LOGIN     VARCHAR(20) NOT NULL,
    USER_PASSWORD  VARCHAR(20) NOT NULL
);




/******************************************************************************/
/****                             Primary Keys                             ****/
/******************************************************************************/

ALTER TABLE CITY ADD CONSTRAINT PK_CITY PRIMARY KEY (ID_CITY);
ALTER TABLE PICTURES ADD CONSTRAINT PK_PICTURES PRIMARY KEY (ID_PICTURE);
ALTER TABLE ROLES ADD CONSTRAINT PK_ROLES PRIMARY KEY (ID_ROLE);
ALTER TABLE USERS ADD CONSTRAINT PK_USERS PRIMARY KEY (ID_USER);
ALTER TABLE USER_ROLE ADD CONSTRAINT PK_USER_ROLE PRIMARY KEY (ID_USER, ID_ROLE);


/******************************************************************************/
/****                             Foreign Keys                             ****/
/******************************************************************************/

ALTER TABLE PICTURES ADD CONSTRAINT FK_PICTURES_CITY FOREIGN KEY (ID_CITY) REFERENCES CITY (ID_CITY);
ALTER TABLE USER_ROLE ADD CONSTRAINT FK_USER_ROLE_ROLE FOREIGN KEY (ID_ROLE) REFERENCES ROLES (ID_ROLE);
ALTER TABLE USER_ROLE ADD CONSTRAINT FK_USER_ROLE_USERS FOREIGN KEY (ID_USER) REFERENCES USERS (ID_USER);


/******************************************************************************/
/****                      Populate Table CITY                             ****/
/******************************************************************************/

INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (1, 'San Francisco', 'CA', 'EUA');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (2, 'São Paulo', 'SP', 'Brazil');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (3, 'Florianópolis', 'SC', 'Brazil');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (4, 'Santa Cruz', 'CA', 'EUA');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (5, 'Rio de Janeiro', 'RJ', 'Brazil');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (6, 'Santo Domingo', 'SD', 'Dominican Republic');
INSERT INTO "CITY" ("ID_CITY", "CITY", "STATE", "COUNTRY") VALUES (7, 'Big Sur', 'CA', 'EUA');


/******************************************************************************/
/****                   Populate Table Pictures                            ****/
/******************************************************************************/

INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (1, 1, 'Golden Gate 1', '~/images/sanfrancisco1.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (2, 2, 'Zoo - Turtle', '~/images/spturtle.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (10, 3, 'Pantano do Sul Beach', '~/images/floripa1.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (11, 3, 'Pantano do Sul Beach', '~/images/floripa2.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (12, 3, 'Street Beira Mar', '~/images/floripa3.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (14, 1, 'Alcatraz 1', '~/images/Alcatraz1.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (13, 1, 'Golden Gate 2', '~/images/sanfrancisco2.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (15, 1, 'Alcatraz 2', '~/images/Alcatraz2.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (16, 4, 'Surf 1', '~/images/SantaCruz1.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (17, 4, 'Surf 2', '~/images/SantaCruz2.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (18, 4, 'Surf 3', '~/images/SantaCruz3.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (19, 5, 'Hang-Glider in San Conrando Beach', '~/images/SanConradoBeach.JPG');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (4, 2, 'Zoo - Elephant', '~/images/elephant.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (20, 6, 'Bocachica Beach', '~/images/BocachicaBeach.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (21, 6, 'RD Beach', '~/images/RD2.JPG');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (3, 2, 'Zoo - Penguin', '~/images/sppenguin.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (5, 2, 'Zoo - Gorila', '~/images/gorila.jpg');
INSERT INTO "PICTURES" ("ID_PICTURE", "ID_CITY", "PICTURE_NAME", "PATH") VALUES (22, 8, 'Bridge', '~/images/IMG_0428.jpg');


/******************************************************************************/
/****                      Populate Table ROLES                            ****/
/******************************************************************************/

INSERT INTO ROLES (ID_ROLE, ROLE_NAME) VALUES (1, 'Administrator');
INSERT INTO ROLES (ID_ROLE, ROLE_NAME) VALUES (2, 'Manager');
INSERT INTO ROLES (ID_ROLE, ROLE_NAME) VALUES (3, 'Operador');

/******************************************************************************/
/****                      Populate Table USERS                            ****/
/******************************************************************************/

INSERT INTO USERS (ID_USER, NAME, USER_LOGIN, USER_PASSWORD) VALUES (1, 'Embarcadero (Administrator)', 'embt', 'delphi');
INSERT INTO USERS (ID_USER, NAME, USER_LOGIN, USER_PASSWORD) VALUES (2, 'Operator', 'operator', 'operator');
INSERT INTO USERS (ID_USER, NAME, USER_LOGIN, USER_PASSWORD) VALUES (3, 'Manager', 'manager', 'manager');

/******************************************************************************/
/****                   Populate Table USER_ROLE                           ****/
/******************************************************************************/

INSERT INTO USER_ROLE (ID_USER, ID_ROLE) VALUES (1, 1);
INSERT INTO USER_ROLE (ID_USER, ID_ROLE) VALUES (1, 2);
INSERT INTO USER_ROLE (ID_USER, ID_ROLE) VALUES (1, 3);
INSERT INTO USER_ROLE (ID_USER, ID_ROLE) VALUES (2, 3);
INSERT INTO USER_ROLE (ID_USER, ID_ROLE) VALUES (3, 3);

COMMIT WORK;

