program sobreviventes
	implicit none


	real(8):: dt, lambda, Tt, lambda0, tm, suntempo, tempo, tempomed, h
	integer:: i, n, n0, n1, k
	
	Tt = 10.d0
	
	read(*,*) n0
	read(*,*) lambda
	read(*,*) dt
	
	tempomed = 1/lambda
	n = n0
	tm = 0.d0

	do i = 0, int(Tt/dt)
		n1 = 0
		do k = 1, n0
			lambda0 = rand()
			if ((lambda0) <= (lambda*dt)) then 
				n1 = n1 + 1
			endif
		enddo
		n0 = n0 - n1
		tm = tm + dt*i*dt*n0*lambda/n
	enddo

	write(*,*)  tm, tempomed
		
end program 	
