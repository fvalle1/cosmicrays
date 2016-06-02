program raggi

!****************************************
!* Simulation of cosmic rays directions *
!* using MonteCarlo's methods           *
!* Results compared with                *
!* real data collected in Turin	        *
!* on December 2012.                    *
!* Program written on September 2013.   *
!****************************************

USE omp_lib  !Libreries
USE par_zig_mod

implicit none
real :: u, v 										!random numbers
integer :: n, h, i, j, seedi
real :: seedr, start, finish
real:: s, probabilita, z 								!Private
integer, parameter :: m = 960, hit = 750000                 !Points
real, allocatable, dimension(:,:) :: theta, phi             !Angles
real, parameter :: pi = 4*atan(1.), e = 2.71828182846, Re = 6371.0, Yatm = 9
!constant
Logical, allocatable, Dimension (:,:) :: mont, get
character*(1000):: fileposition, filenumber, string2, string3, ext 
integer :: len2, len3, status2, status3
character*(1000):: filename

!variables for random
integer, parameter :: grainsize = 32
real(4):: r
real:: time
integer :: len, status, kpar, npar
integer, allocatable :: seed(:)
character*(8) :: str

!example of command line ./raggi 8 Files 1  program threads folder filenumber

n = hit !inizialize n

!To submit, comment next two lines
WRITE(*,*) "How many points?"
READ(*,fmt = '(I15)') n

allocate (theta(m, n))  !allocate array
allocate (phi(m, n))
allocate (mont(m, n))
allocate (get(m, n))

!$ start = omp_get_wtime() !Start time

kpar = 0 !Inizialize kpar for non-parallell machines

!Random
call random_seed()
call get_command_argument(1, str, len, status)
read(str,'(i2)') npar
allocate(seed(npar+1))
!$ call omp_set_num_threads(npar)
call cpu_time(time)
seedr = time*10000
seedi = INT(seedr)
call srand(seedi)
DO h = 1,npar+1
	r = rand()
	seed(h) = r*123+1
ENDDO
call par_zigset(npar+1, seed, grainsize)
!end Random

call get_command_argument(2, string2, len2, status2) 				!Read from command line
!file position without / (ex. for /Files/file.csv write Files)
read(string2,'(A)') fileposition
call get_command_argument(3, string3, len3, status3) !read file number (ex.
!uniform1.csv uniform2.csv 
read(string3,'(A)') filenumber

ext = '.csv'  !Set file extension

filename = fileposition(:len2)//'/uniform-'//filenumber(:len3)//ext    !File opening
open(unit = 1, FILE = filename, STATUS = 'REPLACE')              
filename = fileposition(:len2)//'/atmosphere-'//filenumber(:len3)//ext
open(unit = 2, FILE = filename, STATUS = 'REPLACE')
filename = fileposition(:len2)//'/mountain-'//filenumber(:len3)//ext
open(unit = 3, FILE = filename, STATUS = 'REPLACE')
filename = fileposition(:len2)//'/real-'//filenumber(:len3)//ext
open(unit = 4, FILE = filename, STATUS = 'REPLACE')

WRITE(*,*) "Ready"

!$OMP PARALLEL
!$OMP DO PRIVATE(kpar, u, v, j)              !Uniform
DO i = 1,m    
	!$ kpar = omp_get_thread_num()+1
	DO j = 1,n 
		u = par_uni(kpar)
		v = par_uni(kpar)
		theta(i, j) = ABS(acos(v))      !generate theta, abs is for semisphere
		phi(i, j) = 2*pi*u
	END DO
END DO
!$OMP END DO

!$OMP SINGLE				!Write data
DO i = 1,m
	DO j = 1,n
		WRITE(1,fmt = '(2F10.3,I2)') theta(i, j), phi(i, j)
		get(i,j) = .false.
	END DO
END DO
!$OMP END SINGLE

!$OMP SINGLE
WRITE(*,*) "Ended uniform"
!$OMP END SINGLE

!$OMP DO PRIVATE(kpar, j, z, s, probabilita)   !With atmosphere
DO i = 1,m
	!$ kpar = omp_get_thread_num()+1
	DO j = 1,n            
		z = par_uni(kpar)
		s = SQRT((Re*cos(theta(i, j)))**2+2*Re*Yatm+Yatm**2)-Re*cos(theta(i, j)) !Atmosphere's width
		probabilita = Prob(s)
		IF (z > probabilita) get(i, j) = .true.
	END DO
END DO
!$OMP END DO

!$OMP SINGLE 			!Write data
DO i = 1,m
	DO j = 1,n
		IF (get(i, j)) WRITE(2,fmt = '(2F10.3)') theta(i, j), phi(i, j)
		get(i,j) = .false.
	END DO
END DO
!$OMP END SINGLE

!$OMP SINGLE
WRITE(*,*) "Ended atmosphere"
!$OMP END SINGLE             

!$OMP DO PRIVATE(kpar, j)   !With mountain
DO i = 1,m
!$ kpar = omp_get_thread_num()+1
	DO j = 1,n       
		mont(i, j) = (((theta>1.45).AND.((phi>3.92).AND.(phi<6.28))).OR.((theta>1.23).AND.((phi>0.52).AND.(phi<3.14))))		get(i, j) = .NOT.mont(i, j)
		END DO
END DO
!$OMP END DO

!$OMP SINGLE  			!Write data
DO i = 1,m
	DO j = 1,n
		IF (get(i, j)) WRITE(3,fmt = '(2F10.3)') theta(i, j), phi(i, j)
		get(i,j) = .false.
	END DO
END DO
!$OMP END SINGLE

!$OMP SINGLE
WRITE(*,*) "Ended mountain"
!$OMP END SINGLE     

!$OMP DO PRIVATE(kpar, j, z, s, probabilita)  !With mountain and atmosphere
DO i = 1,m
	!$ kpar = omp_get_thread_num()+1
	DO j = 1,n
		z = par_uni(kpar)
		s = SQRT((Re*cos(theta(i, j)))**2+2*Re*Yatm+Yatm**2)-Re*cos(theta(i, j))         !Atmosphere's width
		probabilita = Prob(s)
		IF (z > probabilita) get(i, j) = .true.
		IF (mont(i,j)) get(i,j) = .false. 
	END DO
END DO
!$OMP END DO

!$OMP SINGLE		!Write data
DO i = 1,m
	DO j = 1,n
		IF (get(i, j)) WRITE(4, fmt = '(2F10.3)') theta(i, j), phi(i, j)
	END DO
END DO
!$OMP END SINGLE

!$OMP END PARALLEL

close(unit = 1)    !File closing
close(unit = 2)
close(unit = 3)
close(unit = 4)

!$ finish = omp_get_wtime() !Finish time

Write(*,fmt = '(A8, F10.2, A8)') "Ended: ", abs(finish-start), " seconds"

CONTAINS

real function Prob(esse)
real:: esse
real, parameter:: a = 1.2 !Factor to try
	Prob = 1 - e**(-a*esse)
end function

end program raggi

!********************************************************************************
!* Copyright (C) 2013  Filippo							*
!*										*
!*    This program is free software: you can redistribute it and/or modify	*
!*    it under the terms of the GNU General Public License as published by	*
!*    the Free Software Foundation, either version 3 of the License, or		*
!*    (at your option) any later version.					*
!*										*
!*    This program is distributed in the hope that it will be useful,		*
!*    but WITHOUT ANY WARRANTY; without even the implied warranty of		*
!*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		*
!*    GNU General Public License for more details.				*
!*										*
!*    You should have received a copy of the GNU General Public License		*
!*    along with this program.  If not, see <http://www.gnu.org/licenses/>.	*
!********************************************************************************
