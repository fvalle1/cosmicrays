![res.png](res.png "Plot")
# cosmicrays
![Fortran](https://img.shields.io/badge/language-Fortran-green.svg)
[![openmp](https://img.shields.io/badge/requires-open--mp-blue.svg)](http://www.openmp.org/)
[![Gplv3](https://img.shields.io/badge/license-GPLv3-red.svg)](https://www.gnu.org/licenses/)

This is intended to simulate the distribution of **cosmic ray** coming throw **atmosphere**

#compile and run
compile: `make`

run: `./raggi.out`

note: you need **openMP** to run in multi proccessors machines.

#how it works
This generates many rays randomly through **all directions**, then the function:
``` fortran
real function Prob(esse)
real:: esse
real, parameter:: a = 1.2 !Factor to try
	Prob = 1 - e**(-a*esse)
end function
```
simulates the decadence caused by atmosphere.



#license
[![Gplv3](https://www.gnu.org/graphics/gplv3-88x31.png "Gpl v3")](https://www.gnu.org/licenses/gpl.txt)

For further details see [LICENSE](LICENSE).
