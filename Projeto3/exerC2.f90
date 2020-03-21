program Pendulo_EulerCromer
	implicit none

	real(8):: Tt, dt, m, l, the, g, ome0, ome, the0, TheGraus, pi, ene, the0Exa, omeExa, theExa, phi
	integer:: i, k 
	
	g = 10.0d0
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
	the0Exa = (TheGraus*180)/pi
	the =  (TheGraus*180)/pi
	phi =  (TheGraus*180)/pi
	
	open(unit = 1, file = "exerC2_out.dat",status = "replace", action = "write")
	!open(unit = 2, file = "energia_pendulo2.dat",status = "replace", action = "write")
	do i = 1, int(Tt/dt)
		if (the > pi) then
			the =  the - 2*pi
		endif
		if (the < (-pi)) then 
			the = the + 2*pi
		end if
		
		ome = ome0 - (g/l)*the0*dt
		the = the0 + ome*dt	
	!	ene = (0.5d0)*m*(l*l)*(ome*ome) + m*g*l*(1-cos(the))
		
		
		ome0 = ome
		the0 = the
		write(1,*) (dt*i), the
	!	write(2,*) (dt*i), ene
	enddo
	close(unit = 1)
	!close(unit = 2)

	!open(unit = 3, file = "exato_out.dat",status = "replace", action = "write")
	!do i = 1,int(Tt/dt)
	!	omeExa = (g/l)**(0.5d0)
	!	theExa = the0Exa*sin(omeExa*(dt*i)+phi)
	!	write(3,*) (dt*i), theExa
	!enddo
		
		
		
		
		
	
end program
