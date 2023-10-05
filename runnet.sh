#!/bin/sh
# sudo docker compose --file cluster/docker-compose.yaml down
# sudo docker compose --file cluster/docker-compose.yaml up -d
# sudo docker compose --file cluster/docker-compose.yaml ps 
# sudo docker compose --file cluster/docker-compose.yaml logs | grep node-query 
# sudo docker compose --file cluster/docker-compose.yaml exec node-query bash

sudo docker compose --file cluster/docker-compose.yaml up 
