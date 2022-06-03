#!/usr/bin/bash

PORT=$3
HOST=$4

ssh -t -p $PORT -L localhost:8888:localhost:8888 $HOST "bash ~/launch_jupyter.sh"
