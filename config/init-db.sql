CREATE TABLE pkeys (
    pkey VARCHAR(255) PRIMARY KEY,
    pubkey VARCHAR(255) NOT NULL ,
    pubkeyhash VARCHAR(255) NOT NULL ,
    balance BIGINT NULL, 
    time TIMESTAMP NULL
);

CREATE TABLE validators (
    id SERIAL ,
    validator BYTEA PRIMARY KEY,
    time TIMESTAMP  
);


CREATE TABLE txlocked (
    txHash BYTEA PRIMARY KEY,
    txOutInd INT,
    valId INT, 
    time TIMESTAMP 
);
