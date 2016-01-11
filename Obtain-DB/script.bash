#!/bin/bash
echo "Downloading Database..."
wget http://stat-computing.org/dataexpo/2009/2001.csv.bz2
bzip2 -d 2001.csv.bz2
python filter.py


