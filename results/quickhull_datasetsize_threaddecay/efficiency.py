#!/usr/bin/env python

'''
Converts a results file into a suitable one for plotting.

File should be of the format:

.......... [x-value]
Line_Label [y-value]

The y value is transformed into efficiency, w.r.t the single threaded run for each line.

This program reads from a file called 'results' and outputs to a file called 'results.efficiency.t'

To collect results (for a benchmark supporting criterion):
Hardcoded in collect.py is the name of binaries to execute. For example for quickhull we have binaries that differ by generated data set size and threshold to enter sequentialism.

cd benchmark
./collect.py > results 2>&1

To generate graphs, execute the following:

./efficiency.py
gnuplot plot.gnu

and the file performanceDecay.pdf will be generated.

Note that the number of lines (which happens to be the number of binaries in the collection phase) is hardcoded and the style of lines is hardcoded to be optimal for quickhull.
'''

'''
E.g. input:

DataSize            1            2            3            4            5            6            7            8            9           10           11           12           13           14
       100000/Seq0 0.0941937400 0.0707407800 0.0588267500 0.0554549900 0.0602186600 0.0765506100 0.0898985100 0.1181375000 0.1434381000 0.1657055000 0.2030599000 0.2246673000 0.2578863000 0.2665433000
     100000/Seq100 0.0869877900 0.0598866800 0.0519817200 0.0419292000 0.0449932200 0.0516193900 0.0674126700 0.0745431200 0.0872138200 0.1007717000 0.1109987000 0.1261829000 0.1443895000 0.1562562000
    100000/Seq1000 0.0863736100 0.0574428400 0.0447947000 0.0368441700 0.0360657500 0.0394714700 0.0456043000 0.0519778900 0.0645671100 0.0732256300 0.0805966600 0.0875732300 0.0933409000 0.0986999300
      1000000/Seq0 0.6796831000 0.3916126000 0.2813248000 0.2369302000 0.2209488000 0.2212179000 0.2243918000 0.2344189000 0.2515953000 0.2844126000 0.3020670000 0.3256904000 0.3743401000 0.3811780000
    1000000/Seq100 0.6691619000 0.3849209000 0.2751861000 0.2217842000 0.1969309000 0.1907788000 0.1769896000 0.1729847000 0.1790351000 0.1803789000 0.1898839000 0.1963313000 0.2133491000 0.2266057000
   1000000/Seq1000 0.6736050000 0.3797991000 0.2678586000 0.2204796000 0.1854264000 0.1735310000 0.1657168000 0.1564823000 0.1588370000 0.1618823000 0.1531407000 0.1616196000 0.1617036000 0.1634039000
      2000000/Seq0 1.4633640000 0.8288617000 0.6170696000 0.5108200000 0.4361772000 0.3993777000 0.4037858000 0.3966236000 0.4004384000 0.4368255000 0.4626833000 0.4790626000 0.5151916000 0.5209859000
    2000000/Seq100 1.4866490000 0.8494575000 0.6002768000 0.4860225000 0.4059742000 0.3714362000 0.3506122000 0.3494826000 0.3346279000 0.3428809000 0.3383959000 0.3455136000 0.3440542000 0.3521715000
   2000000/Seq1000 1.4818040000 0.8440637000 0.5977020000 0.4605800000 0.3989725000 0.3502106000 0.3391692000 0.3190645000 0.3065701000 0.3074087000 0.3147147000 0.3045958000 0.2993717000 0.3183511000
'''

f = open("results", "r")
o = open("results.efficiency", "w")

skipFirstLine = True
for line in f.readlines():
    line = line.strip()
    if skipFirstLine:
        skipFirstLine = False
        o.write(line+"\n")
        continue
    
    vs = line.split()
    base = float(vs[1])
    for v in range(1, len(vs)):
        vs[v] = base / float(vs[v])

    o.write("%20s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s %12.10s\n" % tuple (vs))
o.close()
f.close()

#Transpose

f = open("results.efficiency", "r")
o = open("results.efficiency.t", "w")

rows = []

for line in f.readlines():
    rows.append(line.strip().split())

for row in (map(list, zip(*rows))):
    o.write(" ".join(row)+"\n")
o.close()
f.close()
