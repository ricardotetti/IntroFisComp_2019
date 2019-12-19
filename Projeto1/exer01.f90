program precisao !programa que calucula a precisão de números reais
	
	implicit none
	
	real(4):: a4 = 1.0e0 !variavel com precisão simples
	real(8):: a8 = 1.0d0 !variavel com precisão dupla
	real(16):: a16 = 1.0_16 !variavel com precisao quadrupla
	integer :: c1 = 0,c2 = 0,c3 = 0 !contador 

	do while ((1+a4) /= 1) !Número da mantissa é o a4, número de bits será representado pelo contador c1
		a4 = a4/2
		c1 = c1 + 1
	enddo
	
	do while ((1+a8) /= 1) 
		a8 = a8/2
		c2 = c2 + 1
	enddo

	do while ((1+a16) /= 1) 
		a16 = a16/2
		c3 = c3 + 1
	enddo

	write (*,*) c1, a4 !print do número de bits seguido da variavel com a sua respectiva precisão
	write (*,*) c2, a8
	write (*,*) c3, a16

end program 
