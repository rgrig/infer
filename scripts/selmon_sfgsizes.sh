#!/bin/bash
grep -r SFG_SIZE infer-out/ | sed 's/^.*\/\///'
