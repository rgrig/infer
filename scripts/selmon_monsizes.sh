#!/bin/bash
grep -r MON_SIZE infer-out/ | sed 's/^.*\/\///'
