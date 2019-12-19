program autovec
	implicit none 
	
	real(16) :: eps, l1, l2
	integer :: n,i 
	real, dimension(:,:), allocatable :: matriz
	real, dimension(:), allocatable :: vet, x
	
	read(*,*) eps !precisão escolhida 
	read(*,*) n !dimensão da matriz 

	allocate(vet(n)) !vetor x do enunciado
	allocate(x(n)) !vetor xk, multiplicado m^k vezes
	allocate(matriz(n,n)) !matriz que se quer encontrar o autovetor e autovalor
	read(*,*) matriz

	do i = 1,n !colocar os vetores dentro da matriz
		vet(i) = i	
	enddo
	
	l1 = 0
	x = matmul(matriz,vet)
	do i = 1,30
		vet = x
		x = matmul(matriz,x)
		l2 = dot_product(x,matmul(matriz,x))/dot_product(x,x) !Método das potências
		if (abs(l2-l1) < eps) exit !Caso a diferença dos dois lambdas for menor que o epsilon o loop deve parar e assim o lambda com a maior precisão.
			l1=l2
	enddo
	
	write(*,*) l2 !Valor do lambda mais preciso
	do i = 1,n
		write(*,*) vet(i) !Autovetor associado printado um por linha 
	enddo
end program

	
	
	
	
				
