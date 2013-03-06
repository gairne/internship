#!/usr/bin/env python

import sys
from subprocess import call

args = sys.argv

bench = args[1]
input = args[2]
prog = args[3:] #./Main -s nested-bh -b 20000 --max-steps 10

opts = ["+RTS -N1", "+RTS -N2", "+RTS -N4", "+RTS -N8", "+RTS -N12", "+RTS -N16"]

print prog

def run(prog):
    ex = ["/home/t-mmole/scripts/run3.sh"] + prog #["/home/t-mmole/scripts/run.sh"] + prog# + [">", "~/.tmp", "2>&1"]
    print ("Running " + " ".join(ex))
    call(ex)
    print "finished"
    f = open("/home/t-mmole/.tmp", "r")
    for l in f.readlines():
        if l.startswith("elapsedTimeMS   ="):
            return int(l.strip().split("=")[1].strip()) * 1.0 / 1000.0
#        if l.startswith("real"):
#            m,s = l.strip().split()[1].split("m")
#            return int(m) * 60 + float(s[:-1])

s = "\"problem_name\" : \"" + bench + "\",\n"
s = s + "\"input\" : \"" + input + "\",\n"

for opt in opts:
    p = prog[:]
    p.append(opt)
    s = s + "  { \"n_procs\" : " + str(opt[7:]) + ",   \"time_sec\" : " + str(run(p)) + ",   \"gc\" : []},\n"

print s

xx = open(bench + input + ".log", 'w')
xx.write(s)
xx.close()
