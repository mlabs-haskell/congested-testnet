# Congestion periods from public resourses
- https://cexplorer.io/usage
- Cardano is currently at 94% load!Only 6% left until Cardano is at max capacityWhat does that mean, and how can you be prepared pic.twitter.com/1TZsIUg10N — Sebastien Guillemot (@SebastienGllmt) May 8, 2023

# Collect transaction' statistic from blocks 
- Connect to remote server:  `ssh cardano-cli@node-1.mainnet.ctl-runtime.staging.mlabs.city`. Connect to db: `psql -U cexplorer -d cdbsync`.

--table with transactions 
select * from tx limit 10; 

--all tables and columns

SELECT
    t.tablename AS table_name,
    a.attname AS column_name
FROM
    pg_tables t
JOIN
    pg_attribute a ON t.tablename = a.attrelid::regclass::text
WHERE
    t.tableowner = 'cexplorer'
    AND a.attnum > 0
    AND NOT a.attisdropped
ORDER BY
    t.tablename,
    a.attnum;
