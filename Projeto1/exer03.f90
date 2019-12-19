program taylor
	implicit none 

	real(4) :: sen1, sena1, x1, erro1, precisao1  !variavel com precisão simples 
	real(8) :: sen2, sena2, x2, erro2, precisao2  !variavel com precisão dupla
	integer :: m, i, n, p, fat	
	
	x1 = 0.1e0
	x2 = 0.1d0

	do i = 1,4
		sen2 = x2*i
		sen1 = x1*i
		n = 3
		precisao1 = 1.d0
		precisao2 = 1.d0

		do while (precisao1 > epsilon(0.1e0))
			sena1 = sen1
			sen1 = sen1 + ((-1)**p)*((i*x1)**n)/fat(n)
			p = p + 1
			n = n + 2
			erro1 = abs(((-1)**(p))*((i*x1)**(n))/fat(n))
			precisao1 = erro1/sen1
		enddo

		do p = 1,6 
			sena2 = sen2
			sen2 = sen2 + ((-1)**p)*((i*x2)**n)/fat(n)
			erro2 = abs(((-1)**(p))*((i*x2)**(n))/fat(n))
			precisao2 = erro2/sen2
			if (abs(precisao2)<epsilon(0.1d0)) exit
			n = n + 2 
		enddo
		
		
		write(*,*) (i*x2), precisao1, precisao2
	enddo


end program


function fat(n) !Função para calcular o fatorial da expansão.
	implicit none
	integer, intent(in) :: n
	integer :: i, fact, fat
	fact = 1
	do i = 1, n
		fact = fact*i
	enddo
	fat = fact
end function fat	

!O uso da expansão de taylor para funções trigonométricas parece ser algo bem aceitavel de ser usado, uma vez que a precisão que é alcançada é maior que a função epsilon do fortran.
