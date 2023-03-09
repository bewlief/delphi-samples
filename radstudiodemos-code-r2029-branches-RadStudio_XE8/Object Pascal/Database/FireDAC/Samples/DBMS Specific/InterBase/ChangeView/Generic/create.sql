CREATE DATABASE 'C:\SUB.IB'
USER 'SYSDBA' PASSWORD 'masterkey'
DEFAULT CHARACTER SET UTF8;

CREATE GENERATOR gen;

CREATE TABLE tab (
  id   DECIMAL(10,0) NOT NULL PRIMARY KEY,
  name VARCHAR(20)
);

SET TERM ^ ;
CREATE TRIGGER tr_tab_before_ins FOR tab
ACTIVE BEFORE INSERT POSITION 0
AS
BEGIN
  IF (NEW.id IS NULL) THEN
  BEGIN
    NEW.id = GEN_ID( gen, 1);
  END
END
^

CREATE TRIGGER tr_tab_after_ins FOR tab
ACTIVE AFTER INSERT POSITION 0
AS
BEGIN
  POST_EVENT 'TAB';
END
^

CREATE TRIGGER tr_tab_after_upd FOR tab
ACTIVE AFTER UPDATE POSITION 0
AS
BEGIN
  POST_EVENT 'TAB';
END
^

CREATE TRIGGER tr_tab_after_del FOR tab
ACTIVE AFTER DELETE POSITION 0
AS
BEGIN
  POST_EVENT 'TAB';
END
^
SET TERM ; ^

CREATE SUBSCRIPTION sub ON tab FOR ROW (INSERT, UPDATE, DELETE);

INSERT INTO tab (name) VALUES ('aaa');
INSERT INTO tab (name) VALUES ('bbb');
INSERT INTO tab (name) VALUES ('ccc');
