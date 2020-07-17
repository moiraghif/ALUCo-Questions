#!/usr/bin/bash

ANACONDA_PATH="/home/fede/.anaconda/"
source $ANACONDA_PATH/etc/profile.d/conda.sh

# export JAVA_HOME="$ANACONDA_PATH/envs/tesi/jre/"
# export LD_LIBRARY_PATH="$ANACONDA_PATH/envs/tesi/lib/python3.7/site-packages/jep/"
# export LD_PRELOAD="$ANACONDA_PATH/envs/tesi/lib/libpython3.7m.so.1.0"

conda activate tesi
sbt run
