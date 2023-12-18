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
    txHash BYTEA ,
    txOutInd INT,
    valId INT, 
    time TIMESTAMP ,
    PRIMARY KEY (txHash, txOutInd)
);

CREATE TABLE txRecentlyUsed (
    txHash BYTEA,
    txOutInd INT,
    time TIMESTAMP,
    PRIMARY KEY (txHash, txOutInd)
);
