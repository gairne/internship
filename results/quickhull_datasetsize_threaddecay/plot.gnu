set terminal pdf enhanced
set output 'performanceDecay.pdf'

set xlabel 'threads'
set ylabel 'speedup'

set key left top

set title "Quickhull scalability with different data set lengths"

set style line 1 lc rgb '#FF0000' lt 1 lw 1 pt 1 ps 1.5
set style line 2 lc rgb '#FF4500' lt 1 lw 1 pt 2 ps 1.5
set style line 3 lc rgb '#8B0000' lt 1 lw 1 pt 6 ps 1.5
set style line 4 lc rgb '#008000' lt 1 lw 1 pt 1 ps 1.5
set style line 5 lc rgb '#228B22' lt 1 lw 1 pt 2 ps 1.5
set style line 6 lc rgb '#ADFF2F' lt 1 lw 1 pt 6 ps 1.5
set style line 7 lc rgb '#0000FF' lt 1 lw 1 pt 1 ps 1.5
set style line 8 lc rgb '#1E90FF' lt 1 lw 1 pt 2 ps 1.5
set style line 9 lc rgb '#00CED1' lt 1 lw 1 pt 6 ps 1.5

plot 'results.efficiency.t' using 1:2 title col with linespoints ls 1, \
'' using 1:3 title col with linespoints ls 2, \
'' using 1:4 title col with linespoints ls 3, \
'' using 1:5 title col with linespoints ls 4, \
'' using 1:6 title col with linespoints ls 5, \
'' using 1:7 title col with linespoints ls 6, \
'' using 1:8 title col with linespoints ls 7, \
'' using 1:9 title col with linespoints ls 8, \
'' using 1:10 title col with linespoints ls 9