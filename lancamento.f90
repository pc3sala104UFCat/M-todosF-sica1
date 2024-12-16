program cinematica
implicit none
!------------------------------------------------------------
!Declaracao de variaveis
 real :: x0,v0,t0,tf,dt,t,x,vx,g,t1,t2,y0,vy0,Tetha,Rad,y
!------------------------------------------------------------
!método de entrada de dados:
 print *,'# Entre com x0,y0,v0,Tetha:'
 read  *,x0,y0,v0,Tetha
 print *,'# x0= ',x0,' v0= ',v0
 if(v0 .eq. 0.0               ) stop 'velocidade deve ser diferente de zero.'
 print *,'# Entre t0,dt,tf:'
 read  *,t0,dt,tf
 print *,'# t0= ',t0,' dt= ',dt
!------------------------------------------------------------
!Inicializacao
 t = t0
 g = 9.81
 Rad = 0
 Rad = 2*3.14*(Tetha)/360
 vy0 = sin(Rad)*v0
 vx = cos(Rad)*v0
 open(unit=12,file='caixa3D4.dat')
 do while(t .le. tf)
  write(12,*)x,y
  y = y0 + vy0*t - 0.5*g*(t**2)
  x = x0 + vx*t
  t = t + dt
 enddo
 close(12)
end program cinematica
