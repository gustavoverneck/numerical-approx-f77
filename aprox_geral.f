c------------------------------------------------ FATORIAL
        double precision function f (n)
        integer n, i
        double precision f0
        
        f0 = 1.0d0
        DO i = 1, n, 1
            f0 = f0 * i
        end do
        f = f0
        return
        end
c------------------------------------------------ SIN
        double precision function sins (x, ord)
        double precision x, sinx, f
        integer n, ord
        
        sinx = x
        do n = 1, ord, 1
            sinx = sinx+((-1.0d0)**n*(x**(2.0d0*n+1.0)/f(2*n+1)))
        end do
        sins = sinx
        print*, "###### - Aproximação de sen(x) - ######"
        print*, "sen(x) = x - x**3/3! + x**5/5! - x**7/7! + ..."
        print*, "sin(",x,"): ", sinx
        print*, " "
        return
        end
c------------------------------------------------ COS

        double precision function coss (x, ord)
        double precision x, cosx, f
        integer n, ord

        cosx = 1
        do n = 1, ord, 1
        cosx = cosx + ((-1)**n * (x**(2*n)/f(2*n)))
        end do
        coss = cosx
        print*, "###### - Aproximação de cos(x) - ######"
        print*, "cos(x) = x - x**2/2! + x**4/4! - x**6/6! + ..."
        print*, "cos(",x,"): ", cosx
        print*, " "
        return
        end
c------------------------------------------------ TAN

        double precision function tans (x, ord)
        double precision x, tanx, f, sins, coss
        integer n, ord

        tanx = sins(x, ord)/coss(x, ord)
        tans = tanx
        print*, "###### - Aproximação de tan(x) - ######"
        print*, "tan(",x,"): ", tanx
        print*, " "
        return
        end

c------------------------------------------------

c       MAIN CODE
        program calc_aprox
        double precision a, var, sins, coss, tans, secs
        integer f, ord, escol
        
        print*, " "
        print*, "Insira o valor de x (real): "
        read*, a
        print*, " "
        print*, "Insira a ordem de truncamento da série: "
        read*, ord
c       Escolha
1       print*, "O que deseja calcular?"
        print*, "sen(x) -> 1"
        print*, "cos(x) -> 2"
        print*, "tan(x) -> 3"
        read*, escol
        print*, "Sua escolha: ", escol
        print*, " "
c       sen
        if (escol .EQ. 1) then
            var = sins(a, ord)
c       cos
        else if (escol .EQ. 2) then
            var = coss(a, ord)
c       tan
        else if (escol .EQ. 3) then
            var = tans(a, ord)
        else
            print*, "Escolha Inválida, tente novamente."
            print*, " "
            go to 1
        end if
        end
