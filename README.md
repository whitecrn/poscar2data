# poscar2data
This script can help you transform POSCAR file to lammps data file

This script is written by gfortran. You can use it simply by placing it on your folder. 

First use command: "chmod +x poscar2data"

Second, if you put your "POSCAR" file in the same folder with this srcipt, use command: "./poscar2data", it can automatically change your 
"POSCAR" file to lammps file with the default name "lammps.data"

Particularly, if your "POSCAR" file doesn't named POSCAR, and you want to name the output file name by your own, you can use the command :
"./poscar2data (your input file name) (your output file name)", for example, "./poscar2data POSCAR1 CsPbBr", where "POSCAR1"is the input file
name, and the "CsPbBr" is the outut file name.

GOOD LUCK. If you have any problem, please contact me.
