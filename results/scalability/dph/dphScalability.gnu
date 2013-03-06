set terminal svg enhanced size 1000 1000
set output "dphScale.svg"
set multiplot layout 2,2 title "Speedup of DPH over SEQ"
set xlabel 'processors'
set ylabel 'speedup'
set key left top

set title 'DMM'
plot 'dmmDph.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'Barnes-Hut'
plot 'barneshutDph.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'NQueens'
plot 'nqueensDph.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'QuickHull'
plot 'quickhullDph.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
