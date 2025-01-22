#!/bin/bash
crop=$1
source /opt/bifxapps/miniconda3/etc/profile.d/conda.sh
unset $PYTHONPATH
conda activate r_ml2
echo "$(pwd)"
Rscript /home/glbrc.org/emchasen/southEastWIsnapPlus/sci5/${crop}/${crop}_sciLite.R