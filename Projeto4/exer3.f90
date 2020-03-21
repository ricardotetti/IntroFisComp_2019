program discretizacao
	implicit none
	real(8):: lambda, dt, Tt
	integer:: n0, i
	
	open(unit = 1, file = "decai_in.dat",action = "read")
		read(1,*) n0
		read(1,*) lambda
		read(1,*) dt
		read(1,*) Tt
	close(unit = 1)
	
	open(unit = 2, file = "decai_out.dat",status = "replace", action = "write")
	
	do i = 1, int(Tt/dt)
		n0 = n0 - lambda*dt*n0
		write(2,*) (i*dt), n0
	enddo
	close(unit = 2)
end program
