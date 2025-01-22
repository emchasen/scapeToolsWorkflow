#!/bin/bash
prop=$1
res=$2
source /opt/bifxapps/miniconda3/etc/profile.d/conda.sh
unset $PYTHONPATH
conda activate r_ml2
echo "$(pwd)"
Rscript /home/glbrc.org/emchasen/WI_raster/southEastWI/${prop}${res}/makeRasters_${prop}_${res}m.R