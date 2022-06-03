@echo off

ssh -t -p %3 -L localhost:8888:localhost:8888 %4 "bash ~/launch_jupyter.sh"