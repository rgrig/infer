#!/bin/bash
cat monsizes.data | grep -v "MON_SIZE 1 " | awk '{print $2}' | selmon_avg.py
