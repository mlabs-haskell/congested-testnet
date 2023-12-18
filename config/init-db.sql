CREATE TABLE wallets (
    pkey BYTEA PRIMARY KEY,
    pubkey BYTEA,
    pubkeyhash BYTEA,
    time TIMESTAMP 
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

CREATE TABLE txRecentlyUsed (
    txHash BYTEA PRIMARY KEY,
    txOutInd INT,
    time TIMESTAMP 
);
