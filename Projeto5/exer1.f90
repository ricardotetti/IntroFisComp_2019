program lyapunov	
	implicit none
	integer:: i, n, k
	real(8):: r, x0, eps, d, t, h, x, xeps
	real(8):: soma, plam, lambda, p, f
	real, dimension(60) :: vetor
	
	
	read(*,*) r
	read(*,*) x0
	read(*,*) eps

	open(unit = 1, file = "dist_out.dat",status = "replace", action = "write")
	
!!!!!!!!!!!!!!!!!!! CALCULO DISTANCIA ENTRE DOIS PERIODOS DIFERENTES	
	x = x0
	xeps = x0 + eps
	
	do i = 1,1000
		x = x*r*(1-x)
		xeps = xeps*r*(1-xeps)
		d = abs(xeps - x)
		write(1,*) i, x ,d
	enddo
!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!! REGRESSÃO LINEAR
	do i = 1, 60
		x = x*r*(1-x)
		xeps = xeps*r*(1-xeps)
		f = abs(xeps- x)
		vetor(i) = f
	enddo		
	p = (log(vetor(5)) - log(vetor(4))) 
!!!!!!!!!!!!!!!!!	
!!!!!!!!!!!!!!!!! CALCULO DE LAMBDA PELO SOMATORIO !!!!!!!!!
	soma = 0.d0
	n = 1000
	do k = 1, n
		plam = log(abs(r-2*r*x0))
		soma = soma + plam
	enddo
	
	lambda = soma/(real(n)-1)
	
	write(*,*) p ,lambda
end program
