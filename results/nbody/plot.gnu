set terminal pdf enhanced
set output 'lts-performance.pdf'

set xlabel 'threads'
set ylabel 'time taken (s)'

set key left top

set title "NBody - DPH vs LTS"

set style line 1 lc rgb '#FF0000' lt 1 lw 1 pt 1 ps 1.5
set style line 2 lc rgb '#008000' lt 1 lw 1 pt 1 ps 1.5
set style line 3 lc rgb '#0022BB' lt 1 lw 1 pt 1 ps 1.5
set style line 4 lc rgb '#FF7700' lt 1 lw 1 pt 1 ps 1.5
set style line 5 lc rgb '#FF00FF' lt 1 lw 1 pt 1 ps 1.5

plot 'results' using 1:2 title col with linespoints ls 1, \
'' using 1:3 title col with linespoints ls 2, \
'' using 1:4 title col with linespoints ls 3, \
'' using 1:5 title col with linespoints ls 4, \
'' using 1:6 title col with linespoints ls 5