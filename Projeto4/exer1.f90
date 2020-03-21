program envelhecimento
	implicit none
	real(8):: dt, lambda, Tt, lambda0
	integer:: i, n, n0, k, n1
	
	Tt = 10.d0
	
	read(*,*) n0
	read(*,*) lambda
	read(*,*) dt

	n = n0
	
	open(unit = 1, file = "decai_out.dat",status = "replace", action = "write")
	!Método de Monte Carlo
	
	do i = 0, int(Tt/dt)
		n1 = 0
		do k = 1, n0
			lambda0 = rand()
			if ((lambda0) <= (lambda*dt)) then 
				n1 = n1 + 1
			endif
		enddo
		write(1,*) (dt*i), n0
		n0 = n0 - n1
	enddo
	close(unit = 1)	

end program
	
