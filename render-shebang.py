import os
import os.path
import argparse

subsitutes = {}

parser = argparse.ArgumentParser()
parser.add_argument('--interpreter', action='append')
parser.add_argument('input', nargs='+')
args = parser.parse_args()

for arg in args.interpreter:
    subsitutes[os.path.basename(arg)] = arg

for fname in args.input:
    tmpname = fname + ".tmp"
    with open(fname + ".tmp", "w") as fout:
        with open(fname) as f:
            first = True
            for line in f:
                line = line.rstrip()
                if line.startswith('#!'):
                    line = line[2:]
                    args = line.split()
                    base = os.path.basename(args[0])

                    if base == 'env':
                        args = [subsitutes[args[1]]] + args[2:]
                    else:
                        args[0] = subsitutes[base]
                    line = '#!' + ' '.join(args)
                print(line, file=fout)
                first = False
    os.replace(tmpname, fname)
