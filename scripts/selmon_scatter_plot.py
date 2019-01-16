#!/usr/bin/env python3

from collections import defaultdict
from math import floor
from pathlib import Path

import matplotlib.pyplot as plt
import sys

def main():
  points_by_id = defaultdict(list)
  for line in sys.stdin:
    ws = line.split()
    i = int(ws[1])
    optim = float(ws[3])
    seeall = float(ws[5])
    points_by_id[i].append(optim / seeall)
  points = []
  for rs in points_by_id.values():
    for j in range(len(rs)):
      for i in range(j):
        points.append((rs[i],rs[j]))
        points.append((rs[j],rs[i]))
  xs = [x for x, _ in points]
  ys = [y for _, y in points]
  plt.scatter(xs,ys,alpha=0.7)
  plt.xlim(xmin=0,xmax=1)
  plt.ylim(ymin=0,ymax=1)
  plt.tight_layout()
  p = Path('scatter.png')
  with p.open('w') as out:
    plt.savefig(out, dpi=256)
  plt.clf()

if __name__ == '__main__':
  main()
