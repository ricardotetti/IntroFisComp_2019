program gravitacao
	implicit none
	real(8):: dt, r, rj, rt, v0t, v0j, pi, ms, mj, mt, g, y0t, x0t, x0j, y0j
	real(8):: x1t, y1t, y1j, x1j, x2t, y2t, x2j, y2j, x1s, y1s
	integer:: i 
	
	
	dt = 0.001d0

	r = 1.d0
	rj = 5.2d0
	rt = 4.2d0

	v0t = 6.28d0
	v0j = 2.75d0	
	
	
	pi =  3.14159265359
	ms = 2.0E+30
	mj = 6.0E+24
	mt = 1.9E+27
	g = 6.6741E-11
	
	y0t = 0.d0

	x0t = r
	
	x0j = rj
	
	open(unit = 1, file = "terra.dat",status = "replace", action = "write")
	open(unit = 2, file = "jupiter.dat",status = "replace", action = "write")
	
	y1t = y0t + v0t*dt
	x1t = x0t

	y1j = y0j + v0j*dt
	x1j = x0j
	
	write(1,*) x1t ,y1t
	write(2,*) x1j, y1j


	do i = 2,int((r**1.5)/dt)

		r = ((x1t - x1s)**2 + (y1t - y1s)**2)**0.5
		rj = ((x1t - x1j)**2 + (y1t - y1j)**2)**0.5
		rt = ((x1j - x1s)**2 + (y1j - y1s)**2)**0.5
		
		y2t = 2*y1t - y0t - ((((4*pi*pi)*y1t/(r**3)))*(dt*dt)) - (g*mj*(y1t - y1j)/(rj**3))
		x2t = 2*x1t - x0t - ((((4*pi*pi)*x1t/(r**3)))*(dt*dt)) - (g*mj*(x1t - x1j)/(rj**3))

		y2j = 2*y1j - y0j - ((((4*pi*pi)*y1j/(rt**3)))*(dt*dt)) - (g*mt*(y1j - y1t)/(rj**3))
		x2j = 2*x1j - x0j - ((((4*pi*pi)*x1j/(rt**3)))*(dt*dt)) - (g*mt*(x1j - x1t)/(rj**3))


		write(1,*) x2t, y2t
		write(2,*) x2j, y2j

		y0t = y1t
		y1t = y2t
		x0t = x1t
		x1t = x2t

		y0j = y1j
		y1j = y2j
		x0j = x1j
		x1j = x2j
		
	enddo
	close(unit = 1)

end program	
