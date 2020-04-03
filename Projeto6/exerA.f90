program gravitacao
	implicit none
	real(8):: r, v0, dt, y1, y0, y2, x1, x2, x0, g, ms, pi, t
	integer:: i 
	
	read(*,*) r
	read(*,*) v0
	read(*,*) dt
	
	pi =  3.14159265359
	ms = 2.0E+30
	g = 6.6741E-11
	
	y0 = 0.d0
	t = 0.d0
	
	x0 = r
	
	open(unit = 1, file = "trajA1_out.dat",status = "replace", action = "write")
	
	y1 = y0 + v0*dt
	x1 = x0
	
	write(1,*) 0.d0 ,x0 ,0.d0
	
	do i = 2,int((r**1.5)/dt)
		
		r = (x1*x1+y1*y1)**0.5
		
		y2 = 2*y1 - y0 - ((((4*pi*pi)*y1/(r*r*r)))*(dt*dt))
		x2 = 2*x1 - x0 - ((((4*pi*pi)*x1/(r*r*r)))*(dt*dt))
	
		write(1,*) dt*i, x2, y2
	
		y0 = y1
		y1 = y2
		x0 = x1
		x1 = x2
		
	enddo
	close(unit = 1)
	
	write(*,*) "Foi escolhido para montar os gráficos o valor de dt menor ou&
			& igual a 0.01, caso contarario o programa demonstra uma orbita&
			& instavel"		
	
end program	
	
