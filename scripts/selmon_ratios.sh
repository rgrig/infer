#!/bin/bash
echo Processing costs*.data
cat costs*.data | grep -v "SEEALL 0.0$" | sort -k2 -g | awk '{print $4/$6}' | sort -g > ratios.data
