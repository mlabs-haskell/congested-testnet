#!/bin/sh
DATA=$1
# metrics url like 0.0.0.0:12789
CARDANO_NODE_METRICS_URL=$2
SPAMMER_METRICS_URL=$3

mkdir -p $DATA

cat <<EOF > $DATA/prometheus.yaml 
global:
  scrape_interval: 60s 
  evaluation_interval: 60s 
alerting:
  alertmanagers:
    - static_configs:
        - targets:
rule_files:
scrape_configs:
   - job_name: 'CARDANO-NODE' # To scrape data from the Cardano node
     scrape_interval: 2s
     static_configs:
       - targets: ['${CARDANO_NODE_METRICS_URL}']
   - job_name: 'TX-TIME' 
     scrape_interval: 4s
     static_configs:
       - targets: ['${SPAMMER_METRICS_URL}']
EOF


prometheus \
  --config.file=$DATA/prometheus.yaml \
  --storage.tsdb.path=$DATA \
  --storage.tsdb.retention.time=4320h
