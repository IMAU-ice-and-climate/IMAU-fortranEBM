If lmc is set to 1, measurement errors are simulated on the input data
The errors are introduced in a block in the INPDATA routine in input.f
Neumayer station has different errors than the IMAU AWS, therefore it is done separately

If lmc == 1, almost no data is written because file io is the most intensive part of a program
As only the cumulative melt for each time step is necessary (to create something like an ensemble graph), only OUTACC is called in ebmmodel.f

After the ebmmodel is run, you should run combinemc.sc
This script combines the newest run with all previous runs in a file master_${aws}.txt
It uses a counter in counter_${aws}.txt to keep track of how many runs have been done already
It also uses lock files to prevent parallel processes of interfering. Only one process can combine the runs into the master txt-file at a time

For parallel computing, 'best' way to set this up is as follows:
Some/directory/
Here are the master_${aws}.txt and counter_${aws}.txt files

n additional directories (for n processor cores) in which the model code is located:
Some/directory/EBM1
Some/directory/EBM2
...
Some/directory/EBMn

In each of the .../EBMi directories, you should run
./ebmmodel.sc -aws ${aws}
./combinemc.sc

You could do this in an infi loop to create a lot of data
