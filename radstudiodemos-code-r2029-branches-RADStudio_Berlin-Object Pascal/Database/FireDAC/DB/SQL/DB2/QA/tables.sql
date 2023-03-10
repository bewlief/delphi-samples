SET CMDSEP ;

DROP TABLE "FDQA_All_types";

CREATE TABLE "FDQA_All_types"  (
      "TBIGINT" BIGINT,        
      "TBLOB" BLOB(10485760) NOT LOGGED NOT COMPACT, 
      "TCHAR" CHAR(10), 
      "TCHAR_BIT" CHAR(100) FOR BIT DATA, 
      "TCLOB" CLOB(15728640) NOT LOGGED NOT COMPACT, 
      --"TDATALINK" DATALINK, 
      "TDATE" DATE, 
      "TDECIMAL" DECIMAL(19,4), 
      "TDOUBLE" DOUBLE, 
      "TGRAPHIC" GRAPHIC(100), 
      "TINTEGER" INTEGER, 
      "TLONGVARCHAR" LONG VARCHAR,
      "TLONGVARGRAPHIC" LONG VARGRAPHIC,
      "TREAL" REAL, 
      "TSMALLINT" SMALLINT, 
      "TTIME" TIME, 
      "TTIMESTAMP" TIMESTAMP, 
      "TVARCHAR" VARCHAR(10), 
      "TVARCHAR_BIT" VARCHAR(10) FOR BIT DATA, 
      "TVARGRAPHIC" VARGRAPHIC(100),
      "TDBCLOB" DBCLOB(15728640) NOT LOGGED NOT COMPACT
      );   

DROP TABLE "FDQA_Ascii_types"; 

CREATE TABLE "FDQA_Ascii_types"  (
      "ATSTRING" VARCHAR(5), 
      "ATFLOAT" DOUBLE, 
      "ATNUMBER" SMALLINT, 
      "ATBOOL" SMALLINT, 
      "ATLONGINT" INTEGER, 
      "ATDATE" DATE, 
      "ATTIME" TIME, 
      "ATDATETIME" TIMESTAMP, 
      "ATBLOB" BLOB(10) NOT LOGGED NOT COMPACT, 
      "ATMEMO" LONG VARCHAR );

DROP TABLE "FDQA_Batch_test";

CREATE TABLE "FDQA_Batch_test"  (
      "TINT" INTEGER, 
      "TSTRING" VARCHAR(50), 
      "TBLOB" BLOB(1024) NOT LOGGED NOT COMPACT );

DROP TABLE "FDQA_Blob";      

CREATE TABLE "FDQA_Blob"  (
      "ID" INTEGER NOT NULL GENERATED ALWAYS
      AS IDENTITY ( START WITH +0, INCREMENT BY +1, NO CACHE ),
      "BLOBDATA" BLOB(1073741824) NOT LOGGED NOT COMPACT );

DROP TABLE "FDQA_DB_types"; 

CREATE TABLE "FDQA_DB_types"  (
      "FTSTRING" VARCHAR(50), 
      "FTSMALLINT" SMALLINT, 
      "FTINTEGER" INTEGER, 
      "FTWORD" SMALLINT, 
      "FTBOOLEAN" SMALLINT, 
      "FTFLOAT" DOUBLE, 
      "FTCURRENCY" DECIMAL(19,4), 
      "FTBCD" DECIMAL(19,4), 
      "FTDATE" DATE, 
      "FTTIME" TIME, 
      "FTDATETIME" TIMESTAMP, 
      "FTBYTES" VARCHAR(50) FOR BIT DATA, 
      "FTBLOB" BLOB(1024) NOT LOGGED NOT COMPACT, 
      "FTMEMO" LONG VARCHAR );   

DROP TABLE "FDQA_details_autoinc"; 

DROP TABLE "FDQA_master_autoinc"; 

CREATE TABLE "FDQA_master_autoinc"  (
      "ID1" INTEGER NOT NULL GENERATED ALWAYS
      AS IDENTITY ( START WITH +0, INCREMENT BY +1, NO CACHE ), 
      "NAME1" VARCHAR(20) NOT NULL );   

ALTER TABLE "FDQA_master_autoinc" 
  ADD CONSTRAINT "PK_mast_autoinc" PRIMARY KEY ("ID1");

ALTER TABLE "FDQA_master_autoinc" 
  ADD CONSTRAINT "UK_master_autoinc" UNIQUE ("NAME1"); 

CREATE TABLE "FDQA_details_autoinc"  (
      "ID2" INTEGER NOT NULL GENERATED ALWAYS
      AS IDENTITY ( START WITH +0, INCREMENT BY +1, NO CACHE ), 
      "FK_ID1" INTEGER, 
      "NAME2" VARCHAR(50) );   

ALTER TABLE "FDQA_details_autoinc" 
  ADD CONSTRAINT "PK_det_autoinc" PRIMARY KEY ("ID2");

ALTER TABLE "FDQA_details_autoinc" 
  ADD CONSTRAINT "FK_detaut_mastauto" FOREIGN KEY ("FK_ID1")
  REFERENCES "FDQA_master_autoinc"  ("ID1")
  ON DELETE CASCADE
  ON UPDATE NO ACTION
  ENFORCED           
  ENABLE QUERY OPTIMIZATION;
 
DROP TRIGGER "FDQA_autoinc";

CREATE TRIGGER "FDQA_autoinc"
AFTER UPDATE OF id1 ON "FDQA_master_autoinc"
REFERENCING NEW AS N OLD AS O
FOR EACH ROW MODE DB2SQL
 UPDATE "FDQA_details_autoinc" SET fk_id1 = N.id1 WHERE fk_id1 = O.id1
/

DROP TABLE "FDQA_ForAsync";

CREATE TABLE "FDQA_ForAsync"  (
      "ID" INTEGER, 
      "NAME" VARCHAR(20) );   

DROP TABLE "FDQA_Identity_tab";

CREATE TABLE "FDQA_Identity_tab"  (
      "auto" INTEGER NOT NULL GENERATED ALWAYS
      AS IDENTITY ( START WITH +0, INCREMENT BY +1, NO CACHE ), 
      "DESCR" VARCHAR(50) );   

DROP TABLE "FDQA_LockTable";

CREATE TABLE "FDQA_LockTable"  (
      "ID" INTEGER, 
      "NAME" VARCHAR(100) );   

DROP TABLE "FDQA_MaxLength";

CREATE TABLE "FDQA_MaxLength"  (
      "STR" VARCHAR(255), 
      "MEMOS" LONG VARCHAR,
      "WIDESTR" VARGRAPHIC(255), 
      "BLOBS" BLOB(1073741824) NOT LOGGED NOT COMPACT );   

DROP TABLE "FDQA_NoValsTable";       

CREATE TABLE "FDQA_NoValsTable"  (
      "ID" INTEGER WITH DEFAULT 2000, 
      "NAME" VARCHAR(10) WITH DEFAULT 'hello');   

DROP TABLE "FDQA_Numbers";

CREATE TABLE "FDQA_Numbers"  (
      "DTBYTE" SMALLINT, 
      "DTSBYTE" SMALLINT, 
      "DTINT16" SMALLINT, 
      "DTINT32" INTEGER, 
      "DTINT64" BIGINT, 
      "DTUINT16" DECIMAL(5,0), 
      "DTUINT32" DECIMAL(10,0), 
      "DTUINT64" DECIMAL(20,0), 
      "DTDOUBLE" DOUBLE, 
      "DTCURRENCY" DECIMAL(19,4),
      "DTBCD" DECIMAL(28,14), 
      "DTFMTBCD" DECIMAL(28,14),
      "DTBOOLEAN" DECIMAL(2,0) );   

DROP TABLE "FDQA_TabWithPK";

CREATE TABLE "FDQA_TabWithPK"  (
      "F1" INTEGER NOT NULL,
      "F2" varchar(2000) );   

ALTER TABLE "FDQA_TabWithPK"
  ADD CONSTRAINT "PK_tabwpk" PRIMARY KEY ("F1");

DROP TABLE "FDQA_TransTable";

CREATE TABLE "FDQA_TransTable"  (
      "ID" INTEGER NOT NULL WITH DEFAULT 2000,
      "NAME" VARCHAR(100) WITH DEFAULT '' );

ALTER TABLE "FDQA_TransTable"
  ADD CONSTRAINT "PK_TransTable" PRIMARY KEY ("ID");

DROP TABLE "FDQA_WString";

CREATE TABLE "FDQA_WString" (
      "WIDESTRING" VARGRAPHIC (1000));

DROP TABLE "FDQA_Bcd";

CREATE TABLE "FDQA_Bcd"  (
      "FTCURRENCY" DECIMAL(19,4), 
      "FTBCD" DECIMAL(19,4), 
      "FTFMTBCD" DECIMAL(22,4) );

DROP TABLE "FDQA_ParamBind";

CREATE TABLE "FDQA_ParamBind" (
  "P1" VARCHAR(50),
  "P2" VARCHAR(50),
  "P3" VARCHAR(50),
  "P4" VARCHAR(50) );

DROP TABLE "FDQA_map1";

CREATE TABLE "FDQA_map1"  (
  "ID1" INTEGER NOT NULL , 
  "NAME1" VARCHAR(20) );

DROP TABLE "FDQA_map2";

CREATE TABLE "FDQA_map2"  (
  "ID2" INTEGER NOT NULL , 
  "NAME2" VARCHAR(20) );

DROP TABLE "FDQA_map3";
 
CREATE TABLE "FDQA_map3"  (
  "ID3" INTEGER NOT NULL , 
  "NAME3" VARCHAR(20) );   

DROP TABLE "FDQA_map4";

CREATE TABLE "FDQA_map4"  (
  "ID4" INTEGER NOT NULL , 
  "NAME4" VARCHAR(20) );

DROP TABLE "FDQA_FK_tab";

CREATE TABLE "FDQA_FK_tab" (
  id int NOT NULL PRIMARY KEY,
  fk_id int,
 CONSTRAINT FK_FDQA_FK_tab FOREIGN KEY (fk_id) REFERENCES "FDQA_FK_tab"(id)
);
 
DROP VIEW "FDQA_V_Test";

CREATE VIEW "FDQA_V_Test" AS
SELECT p.ProductID, p.ProductName, p.UnitPrice * p.UnitsInStock as TotalPrice,
  c.CategoryID, c.CategoryID as CID, c.CategoryName
FROM "Products" p left join "Categories" c on p.CategoryID = c.CategoryID;
