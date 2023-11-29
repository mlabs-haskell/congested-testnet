CREATE TABLE pkeys (
    pkey VARCHAR(255) PRIMARY KEY,
    pubkey VARCHAR(255) NOT NULL ,
    pubkeyhash VARCHAR(255) NOT NULL ,
    balance BIGINT NULL, 
    time TIMESTAMP NULL
);

