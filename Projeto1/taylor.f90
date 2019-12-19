          program serie
             implicit none 
             integer*8  N, i, j
             real*8  serie, x, xrad
             real*8 xtest, pi, senx, func, ten, fat
             write(*,*)"Choose en angle in degrees"
             read(5,*) x
             fat = 1.d0
             ten = 10**(-15)
             senx = 0.d0
             pi= datan2(0.d0,-1.d0)
             xrad = (x*pi)/180.d0
             xtest = (x*pi)/180.d0
             func =dsin(xtest)
             i = 0
             do while (serie >= ten)
                   do j = 1, 2*i +1
                      fat = fat*j
                   end do
                   N = (2*i)+1
                   serie = (((-1)**i)*(xrad**N))/fat
                   senx = senx + serie
                   i = i + 1
                   fat = 1
             end do
             write(*,*)senx
             write (*,*) i
             write(*,*)func
          end program 

