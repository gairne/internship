#!/usr/bin/env python

import sys

data = {}
# data = {"smvm": {"lts 1": {1: 1.324, 2: 0.675}, "ets 128": {1: 1.542, 2: 1.010}}}

def readin(filenm, data):
    f = open(filenm, 'r')
    b, cp, n, t = "", "", 0, 0.00
    for line in f.readlines():
        line = line.strip()
        if line.startswith("\"problem_name"):
            b = line.split(":")[1].strip()[1:-2]
            if b in data:
                print "[ADD] b=%s" % (b,)
            else:
                print "[NEW] b=%s" % (b,)
                data[b] = {}
        elif line.startswith("\"input"):
            cp = line.split(":")[1].strip()[1:-2]
            if b == "":
                print "[WARN] empty benchmark (for cp)"
            if cp in data[b]:
                print "[WARN] chunking policy %s already exists for benchmark %s in dataset %s" % (cp, b, str(data))
            data[b][cp] = {}
            print "[ADD] cp=%s, b=%s" % (cp, b)
        elif line.startswith("{ \"n_procs"):
            if b == "":
                print "[WARN] empty benchmark (for np)"
            if cp == "":
                print "[WARN] empty chunking policy (for np)"
            n = int(line.split(":")[1].strip().split(",")[0])
            t = float(line.split(":")[2].strip().split(",")[0])
            if n in data[b][cp]:
                print "[WARN] data for proc %d = %f already exists for benchmark %s, chunking policy %s in dataset %s" % (n, t, b, cp, str(data))
            data[b][cp][n] = t
            print "[ADD] n=%d, t=%f, cp=%s, b=%s" % (n, t, cp, b)
    
    return data

files = sys.argv[1:]

for filenm in files:
    data = readin(filenm, data)

print data

for benchmark in data:
    chunkings = data[benchmark].keys()
    chunkings.remove("SEQ")
    chunkings.sort()
    if (chunkings != ['Run1','Run2','Run3']):#, 'ETS 128', 'ETS 16384', 'LTS 1']):
        print "[ERR] expected chunkings of ['ETS 1', 'ETS 128', 'ETS 16384', 'LTS 1']"
        sys.exit(1)
    nprocs = data[benchmark][chunkings[0]].keys()
    nprocs.sort()
    f = open ("%s.dat" % (benchmark,), 'w')
    f.write("#  %8s %8s %8s\n" % (chunkings[0], chunkings[1], chunkings[2]))#, chunkings[3]))
    if "SEQ" not in data[benchmark]:
        print "[ERR] benchmark %s missing base" % (benchmark,)
        sys.exit(1)
    base = data[benchmark]["SEQ"][0]
    print "Base time: %s %f" % (benchmark,base)
    for proc in nprocs:
        print proc,
        wline = "%2d " % (proc,)
        for chunking in chunkings:
            if chunking == "SEQ":
                continue
            wline += "%f " % (base/data[benchmark][chunking][proc],)
        f.write(wline+"\n")
    print ""
    f.close()
