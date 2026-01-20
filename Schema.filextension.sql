CREATE DATABASE IF NOT EXISTS Cars;
USE Cars;

-- Table: cars
CREATE TABLE cars (
  carid CHAR(7) NOT NULL,
  makemodel VARCHAR(70),
  reg_year VARCHAR(4),
  km_km INT,
  km_per_l DOUBLE,
  sold BOOLEAN DEFAULT FALSE,
  PRIMARY KEY (carid)
) ENGINE=InnoDB;

-- Table: price_history
CREATE TABLE price_history (
  carid CHAR(7) NOT NULL,
  scrapedate DATE NOT NULL,
  price DOUBLE,
  PRIMARY KEY (carid, scrapedate),
  CONSTRAINT fk_price_car
    FOREIGN KEY (carid)
    REFERENCES cars(carid)
    ON UPDATE CASCADE
    ON DELETE RESTRICT
) ENGINE=InnoDB;
