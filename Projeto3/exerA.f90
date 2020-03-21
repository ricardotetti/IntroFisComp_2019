program bicicleta
	implicit none
	real(8) :: v, t, v0, P, m, dt, Ti
	integer :: k, i 

	m = 70.d0
	P = 400.d0
	
	read(*,*) v0
	read(*,*) dt
	read(*,*) Ti
	
	k = 1 + int(Ti/dt)
	
	open(unit = 1, file = "VelA_out.dat",status = "replace", action = "write")
	!write(*,*) "0 ", "v0"
	do i = 0, (k-1)
		v = v0 + ((P/(m*v0))*(dt))
		v0 = v	
	!	write(*,*) (dt*i), v
		write(unit=1,fmt=*) (dt*i), v
	enddo
	close(unit=1)
end program


