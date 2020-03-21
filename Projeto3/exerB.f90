program bicicleta_atrito
	implicit none
	
	real(8) :: v, t, v0, P, m, dt, Tt, rho, C, A, s, TempoT
	real(8) :: espa, ace, h, trap1, vfun
	integer :: k, i, cont
	
	
	C = 0.5d0
	m = 70.d0
	P = 400.d0
	rho = 1.2d0
	
	!write(*,*) "Velocidade inicial: "
	read(*,*) v0
	!write(*,*) "Intervalo de tempo: "
	read(*,*)  dt
	!write(*,*) "Tempo total: "
	read(*,*)  Tt
	!write(*,*) "Area: "
	read(*,*) A
	
	k = 1 + int(Tt/dt)
	
	!open(unit = 1, file = "VelB3_out.dat",status = "replace", action = "write")
	!do i = 1, int(Tt/dt)
	!	v = v0 + ((P/(m*v0))*(dt)) - ((C*A*rho*(v0*v0)*dt)/m)
	!	v0 = v	
	!	write(1,*) (dt*i), v
	!enddo
	!close(unit = 1)
	
	s = 0.d0
	do i = 1, int(Tt/dt)
		v = v0 + ((P/(m*v0))*(dt)) - ((C*A*rho*(v0*v0)*dt)/m)	
		s = s + (v0*dt)
		if ((v0/v) == 1) exit
		v0 = v
		TempoT = dt*i
	enddo

	!trap1 = 0.d0 !Soma das partições
	!h = 1.d0/4096.d0
	!do i = 1, 4095 !Loop de integração da velocidade para achar o espaço pelo metodo do trapezio
	!	trap1 = trap1 + vfun(i*h)
	!enddo
	!s = (h/2)*(vfun(0.d0) + 2*(trap1) + vfun(Tt))
	
	
	write(*,*) "O ciclista se curva durante a corrida para &
			&conservar o , os ciclistas preferem correr&
			&em grupo para que o atrito com o ar seja menor&
			&, e é mais vantajoso para a ultrapassagem colar&
			& no ciclista da frente devido ao atrito com o ar ser&
			& menor e ele atingir uma velocidade limite maior"
	write(*,*) TempoT
	write(*,*) s
	write(*,*) v
	write(*,*) (s/Tt)
	
end program

!function vfun(dt)
!	implicit none 
!	real(8):: vfun, v0, P, m, dt, C, A, rho
!	vfun = v0 + ((P/(m*v0))*(dt)) - ((C*A*rho*(v0*v0)*dt)/m)	
!end function
