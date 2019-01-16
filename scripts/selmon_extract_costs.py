#!/usr/bin/env python3

from collections import defaultdict
from pathlib import Path

import re
import sys

def main():
  solfn = re.compile(r'.*/([0-9]+)\.([a-z]+)\.sol')
  costs = defaultdict((lambda: {}))
  dbdir = Path('gurobi-out')
  for solfile in dbdir.glob('*.sol'):
    m = solfn.match(str(solfile))
    if m:
      i = int(m.group(1))
      t = m.group(2).upper()
      with solfile.open() as sf:
        for solline in sf:
          if solline.startswith('c0 '):
            ws = solline.split()
            c = float(ws[1])
            break
      costs[i][t] = c
    else:
      sys.stderr.write('MALFORMED solfilename: {}\n'.format(solfile))
  for i, cs in costs.items():
    sys.stdout.write('ID {}'.format(i))
    for t, c in sorted(cs.items()):
      sys.stdout.write(' {} {}'.format(t, c))
    sys.stdout.write('\n')

if __name__ == '__main__':
  main()
