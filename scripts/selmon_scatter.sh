#!/bin/bash
cat costs*.data | grep -v "OPTIM 0.0" | sort -k2 -g > scatter.data
