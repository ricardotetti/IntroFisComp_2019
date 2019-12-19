function fat(n)
	implicit none
	integer, intent(in) :: n
	integer :: i, fact, fat
	fact = 1
	do i = 1, n
		fact = fact*i
	enddo
	fat = fact
end function fat	

function expa(n)






end function expa

program taylor
	implicit none 

	real(4) :: sen1, sena1, x, erro1, precisao1  !variavel com precisão simples, sendo o sen1 
	!real(8) :: sen2, sena2, y, erro2  !variavel com precisão dupla
	integer :: m, i, n, p, fat	
	
	x = 0.1e0
	
	write(*,*) "Precisão simples:"
	do i = 1,4
		sen1 = x*i
		n = 3 !potencia e fatorial incial, como na serie de taylor o primeiro valor elevado a algo é 3 e é incrementado 2 a cada expansão
		do p = 1,10
			sena1 = sen1
			sen1 = sen1 + ((-1)*p)*((i*x)**n)/fat(n)	!serie de taylor com a função fatorial
			if (sen1-sena1 == 0) exit	
			n = n + 2 !incremento
		enddo
	erro1 = abs(((-1)*(p-1))*((i*x)**n))
	precisao1 = erro1/sen1
	write(*,*) (i*x), precisao1
	enddo
end program
