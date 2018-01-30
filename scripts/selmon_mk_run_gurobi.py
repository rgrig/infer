#!/usr/bin/env python3

from pathlib import Path

import re
import sys

def main():
  lpfn = re.compile(r'.*-([0-9]+)\.([a-z]+)\.lp')
  dbdir = Path('infer-out/paths')
  with Path('run_gurobi.sh').open('w') as out:
    for lpfile in dbdir.glob('*.lp'):
      m = lpfn.match(str(lpfile))
      if m:
        solfile = 'gurobi-out/{}.{}.sol'.format(m.group(1), m.group(2))
        out.write("gurobi_cl ResultFile={} '{}'\n".format(solfile, lpfile))
      else:
        out.write('# ERROR: malformed lp filename: {}\n'.format(lpfile))

if __name__ == '__main__':
  main()
