set terminal svg enhanced size 1000 1000
set output "repaScale.svg"
set multiplot layout 2,2 title "Speedup of Repa over SEQ"
set xlabel 'processors'
set ylabel 'speedup'
set key left top

set title 'DMM'
plot 'dmmRepa.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'Barnes-Hut'
plot 'barneshutRepa.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'NQueens'
plot 'nqueensRepa.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
set title 'QuickHull'
plot 'quickhullRepa.dat' using 1:2 title 'Run 1' with linespoints, \
'' using 1:3 title 'Run 2' with linespoints, \
'' using 1:4 title 'Run 3' with linespoints
#
