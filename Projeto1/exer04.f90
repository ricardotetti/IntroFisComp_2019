program ordenamento
	implicit none 
	
	integer :: n, m, i !Variaveis que o programa vai ler
	logical :: troca = .True. !Condição para realizar o sort
	real*8 :: aux
	real, dimension(:), allocatable :: vetor
	read(*,*) n !Número de elementos do arquivo que vão ser ordenados 
	read(*,*) m !Número de elementos que vão aparecer no arquivo novo

	open(unit = 1, file="ord_in.dat") 
	allocate(vetor(n))
	do i = 1, n !Pega os dados do arquivo e passa para o vetor 
		read(1,*) vetor(i)
	enddo
	close(unit = 1)

!***********inicio bubble sort***********
	do while (troca)
		troca = .False.
		do i = 1, n-1
			if (vetor(i).GT.vetor(i+1)) then
				aux = vetor(i)
				vetor(i) = vetor(i+1)
				vetor(i+1) = aux
				troca = .True.	
			endif
		enddo
	enddo
!*********fim bubble sort***********
	open(unit = 2, file = "ord_out.dat",status = "new", action = "write")
	do i = n-m,n-1 
		write(unit = 2, fmt=*) vetor(i) !escreve os m maiores números do arquivo inical
	enddo
	close(unit = 2)
end program
