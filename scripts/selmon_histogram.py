#!/usr/bin/env python3

from math import floor
from pathlib import Path

import matplotlib.pyplot as plt
import sys

def load_data(fn):
  with open(fn) as df:
    return [float(x) for x in df.read().split()]

def bin_it(data, n):
  delta = 1 / n
  xs = [delta * (1/2 + i) for i in range(n)]
  c = { i : 0 for i in range(n) }
  for d in data:
    b = floor(n * d)
    if b == n:
      b = n - 1
    c[b] += 1
  s = sum(c.values())
  return (xs, list(y/s for y in c.values()))

def main():
  plt.rc('font', size=6)
  plt.figure(figsize=(2,2))
  plt.locator_params(axis='x', nbins=5)
  plt.locator_params(axis='y', nbins=1)
  xs = load_data(sys.argv[1])
  xs, ys = bin_it(xs, 30)
  plt.plot(xs,ys,linewidth=1,alpha=0.7)
  plt.xlim(xmin=0,xmax=1)
  plt.ylim(ymin=0)
  plt.xlabel('$c_{\\it inf}/{\\rm Ex}(C_{\\circ})$')
  plt.ylabel('normalized frequency')
  plt.tight_layout()
  p = Path('histo.png')
  with p.open('w') as out:
    plt.savefig(out, dpi=256)
  plt.clf()

if __name__ == '__main__':
  main()
