set terminal svg enhanced size 1000 1000
set output "file.svg"
set multiplot layout 3,2 title "Speedup of ETS and LTS over SEQ"
#3,2
set xlabel 'processors'
set ylabel 'speedup'
set key left top
#set term png
#set output plots.png

set title 'Barnes Hut'
plot 'barnes-hut.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'barnes-hut.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'barnes-hut.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'barnes-hut.dat' using 1:5 title 'LTS' with linespoints
#
set title 'Raytracer'
plot 'raytracer.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'raytracer.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'raytracer.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'raytracer.dat' using 1:5 title 'LTS' with linespoints
#
set title 'Quicksort'
plot 'quicksort.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'quicksort.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'quicksort.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'quicksort.dat' using 1:5 title 'LTS' with linespoints
#
set title 'Smvm'
plot 'smvm.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'smvm.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'smvm.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'smvm.dat' using 1:5 title 'LTS' with linespoints
#
set title 'Dmm'
plot 'dmm.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'dmm.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'dmm.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'dmm.dat' using 1:5 title 'LTS' with linespoints
#
set title 'Nestedsums'
plot 'nestedsums.dat' using 1:2 title 'ETS 2^0' with linespoints, \
     'nestedsums.dat' using 1:3 title 'ETS 2^7' with linespoints, \
     'nestedsums.dat' using 1:4 title 'ETS 2^14' with linespoints, \
     'nestedsums.dat' using 1:5 title 'LTS' with linespoints