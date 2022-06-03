-- $Id: company_schema.sql,v 1.4 2007/06/14 11:10:08 almhe Exp $
-- The Jonson Brothers Ltd. company database 

-- To load this file many times, put the
-- following commands in the beginning of 
-- the file:
 
DROP TABLE IF EXISTS jbsale CASCADE;
DROP TABLE IF EXISTS jbsupply CASCADE;
DROP TABLE IF EXISTS jbdebit CASCADE;
DROP TABLE IF EXISTS jbparts CASCADE;
DROP TABLE IF EXISTS jbitem CASCADE;
DROP TABLE IF EXISTS jbsupplier CASCADE;
DROP TABLE IF EXISTS jbdept CASCADE;
DROP TABLE IF EXISTS jbstore CASCADE;
DROP TABLE IF EXISTS jbcity CASCADE;
DROP TABLE IF EXISTS jbemployee CASCADE;

SELECT 'Creating tables' AS 'Message';

CREATE TABLE jbemployee
   (id INT,
    name VARCHAR(20),
    salary INT,
    manager INT,
    birthyear INT,
    startyear INT,
    CONSTRAINT pk_employee PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbdept (
    id INT,
    name VARCHAR(20),
    store INT NOT NULL,
    floor INT,
    manager INT,
    CONSTRAINT pk_dept PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbitem (
    id INT,
    name VARCHAR(20),
    dept INT NOT NULL,
    price INT,
    qoh INT UNSIGNED /* or, if check constraints were enforced: INT CHECK (qoh >= 0)*/,
    supplier INT NOT NULL,
    CONSTRAINT pk_item PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbparts (
    id INT,
    name VARCHAR(20),
    color VARCHAR(8),
    weight INT,
    qoh INT,
    CONSTRAINT pk_parts PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbsupply (
    supplier INT NOT NULL,
    part INT NOT NULL,
    shipdate DATE NOT NULL,
    quan INT,
    CONSTRAINT pk_supply PRIMARY KEY(supplier, part, shipdate)) ENGINE=InnoDB;

CREATE TABLE jbsale (
    debit INT NOT NULL,
    item INT NOT NULL,
    quantity INT,
    CONSTRAINT pk_sale PRIMARY KEY(debit, item)) ENGINE=InnoDB;

CREATE TABLE jbdebit (
    id INT,
    sdate TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    employee INT NOT NULL,
    account INT NOT NULL,
    CONSTRAINT pk_debit PRIMARY KEY(id)) ENGINE=InnoDB;


CREATE TABLE jbcity (
    id INT,
    name VARCHAR(15),
    state VARCHAR(6),
    CONSTRAINT pk_city PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbstore (
    id INT,
    city INT NOT NULL,
    CONSTRAINT pk_store PRIMARY KEY(id)) ENGINE=InnoDB;

CREATE TABLE jbsupplier (
    id INT,
    name VARCHAR(20),
    city INT NOT NULL,
    CONSTRAINT pk_supplier PRIMARY KEY(id)) ENGINE=InnoDB;

-- Add foreign keys 
SELECT 'Creating foreign keys' AS 'Message';
ALTER TABLE jbdept ADD CONSTRAINT fk_dept_store FOREIGN KEY (store) REFERENCES jbstore(id);
ALTER TABLE jbdept ADD CONSTRAINT fk_dept_mgr FOREIGN KEY (manager) REFERENCES jbemployee(id) ON DELETE SET NULL;

ALTER TABLE jbitem ADD CONSTRAINT fk_item_dept FOREIGN KEY (dept) REFERENCES jbdept(id);
ALTER TABLE jbitem ADD CONSTRAINT fk_item_supplier FOREIGN KEY (supplier) REFERENCES jbsupplier(id);

ALTER TABLE jbsupply ADD CONSTRAINT fk_supply_supplier FOREIGN KEY (supplier) REFERENCES jbsupplier(id);
ALTER TABLE jbsupply ADD CONSTRAINT fk_supply_parts FOREIGN KEY (part) REFERENCES jbparts(id);

ALTER TABLE jbsale ADD CONSTRAINT fk_sale_item FOREIGN KEY (item) REFERENCES jbitem(id);
ALTER TABLE jbsale ADD CONSTRAINT fk_sale_debit FOREIGN KEY (debit) REFERENCES jbdebit(id);
-- implies that a debit/transaction must be created before a sale record.

ALTER TABLE jbdebit ADD CONSTRAINT fk_debit_employee FOREIGN KEY (employee) REFERENCES jbemployee(id);

ALTER TABLE jbstore ADD CONSTRAINT fk_store_city FOREIGN KEY (city) REFERENCES jbcity(id);

ALTER TABLE jbsupplier ADD CONSTRAINT fk_supplier_city FOREIGN KEY (city) REFERENCES jbcity(id);
