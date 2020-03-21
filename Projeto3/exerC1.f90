program Pedulo_Euler
	implicit none
	real(8):: Tt, dt, m, l, the, g, ome0, ome, the0, TheGraus, pi, ene
	integer:: i, k 
	
	g = 10.d0
	ome0 = 0.d0
	pi = 3.14159265359
	
	!write(*,*) "Tempo total: " 
	read(*,*) Tt
	
	!write(*,*) "Delta t:"
	read(*,*) dt
	
	!write(*,*) "Massa:"
	read(*,*) m
	
	!write(*,*) "Comprimento da haste l:"
	read(*,*) l
	
	!write(*,*) "Angulo inicial em graus:"
	read(*,*) TheGraus
	
	the0 = (TheGraus*180)/pi
	the = (TheGraus*180)/pi
	
	
	open(unit = 1, file = "exerC1_out.dat",status = "replace", action = "write")
	!open(unit = 2, file = "energia_pendulo.dat",status = "replace", action = "write")
	
	do i = 1, int(Tt/dt)
		!if (the > pi) then
		!	the =  the - 2*pi
		!endif
		!if (the < (-pi)) then 
		!	the = the + 2*pi
		!end if
	
		ome = ome0 - (g/l)*(the0)*dt
		the = the0 + ome0*dt	
		!ene = ((0.5d0)*(m*(l*l)*(ome*ome))) + m*g*l*(1-cos(the0))
		
		
		ome0 = ome
		the0 = the
		write(1,*) (dt*i), the
	!	write(2,*) (dt*i), ene
		
	enddo
	close(unit = 1)
	!close(unit = 2)
	
end program
