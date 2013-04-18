#!/usr/bin/env python

import subprocess

def run(f, s):
    e = subprocess.check_output(("./repaE"+str(f)+" +RTS -N"+str(s)).split(), stderr=subprocess.STDOUT)
    on = False
    for line in e.split("\n"):
        line = line.strip()
        if (line == "benchmarking QuickHull/Repa"):
            on = True
        if (on and line.startswith("mean: ")):
            return line.split()[1] + line.split()[2][:-1]

def out(s):
    print s

out("%20s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s" % ("DataSize", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11" ,"12", "13", "14"))
for x in ["1_100000_Seq0", "1_1000000_Seq0", "1_2000000_Seq0", "1_100000_Seq100", "1_1000000_Seq100", "1_2000000_Seq100", "1_100000_Seq1000","1_1000000_Seq1000","1_2000000_Seq1000"]:
    s = "%20s " % (str(x),)
    for y in [1,2,3,4,5,6,7,8,9,10,11,12,13,14]:
        r = run(x,y)
        if (r.endswith("ms")):
            r = float(r[:-2])/1000.0
        elif (r.endswith("s")):
            r = float(r[:-1])
        s = s + "%12.10f " % (r,)
    out(s)
