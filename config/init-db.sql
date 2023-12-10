CREATE TABLE pkeys (
    pkey VARCHAR(255) PRIMARY KEY,
    pubkey VARCHAR(255) NOT NULL ,
    pubkeyhash VARCHAR(255) NOT NULL ,
    balance BIGINT NULL, 
    time TIMESTAMP NULL
);

CREATE TABLE scripts (
    script BYTEA PRIMARY KEY,
    time TIMESTAMP NULL
);


CREATE TABLE tx (
    txhash BYTEA PRIMARY KEY,
    id INT NOT NULL,
    pkey BYTEA ,
    time TIMESTAMP 
);
