program stirling
	implicit none
	integer :: i,n = 1, aux !i são os possiveis expontes ate o maior numero ser atingido e n é o maior numero
	integer :: c = 1, p  !c é um contador para mostrar o maior fatorial, fat é o valor do maior fatorial e p indica 
	integer :: fat = 1, fatmax
	integer :: fact = 1, k = 1 !fact é o fatorial dos k numeros para calcular o numero de stirling e o s é o número de stirling
	real :: s
	REAL, PARAMETER :: Pi = 3.1415927
	
	do i = 1, 1000 !Calcula o N limite 
		n = 2**(i)-1
		if (n<0) exit
	enddo
	
	do while (mod(fat,c) == 0) !Calcula qual é o maior fatorial e o seu respectivo valor	
		c = c + 1
		fatmax = fat
		fat = c*fat
	enddo
	
	write(*,*) (2**(i-1)-1), fatmax, c-1 !Escreve o N limite seguido do maior fatorial e seu valor

	do k = 1,c-1 !calcula o fatorial do um numero e o numero de stirling
		fact = fact*real(k)
		s = real(k)*log(real(k)) - real(k) + ((1/2)*log(2*Pi*real(k)))
		write(*,*) k, fact, s
	enddo
	

end program 
