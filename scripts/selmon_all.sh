#!/bin/bash
rm -rf infer-out/paths
infer report --sfg-output --sfg-selmon foo
selmon_sfgsizes.sh | sort -g -k4 > sfgsizes.data
selmon_monsizes.sh | sort -g -k4 > monsizes.data
#exit 0
rm -rf gurobi-out
mkdir -p gurobi-out
selmon_mk_run_gurobi.py
bash run_gurobi.sh
selmon_extract_costs.py > costs.data
selmon_ratios.sh
