#!/usr/bin/env python3

from math import exp, log
from statistics import mean, median

import sys

def main():
  xs = [float(x) for x in sys.stdin.read().split()]
  sys.stdout.write('n {}'.format(len(xs)))
  sys.stdout.write(' min {}'.format(min(xs)))
  sys.stdout.write(' med {}'.format(median(xs)))
  sys.stdout.write(' a-avg {}'.format(mean(xs)))
  sys.stdout.write(' g-avg {}'.format(exp(sum(log(x) for x in xs)/len(xs))))
  sys.stdout.write(' max {}'.format(max(xs)))
  sys.stdout.write(' sum {}'.format(sum(xs)))
  sys.stdout.write('\n')

if __name__ == '__main__':
  main()
