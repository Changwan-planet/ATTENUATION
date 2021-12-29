PROGRAM ATTENUATION
IMPLICIT NONE

REAL*8,PARAMETER :: pi = Acos(-1.0) 
INTEGER, PARAMETER :: N = 10
REAL*8, PARAMETER :: permit = 8.854 * 10.0 **(-12) 
REAL*8, PARAMETER :: permia = 4.0 * pi * 10.0 ** (-7)
!REAL*8 :: cc = 1.0 / sqrt(permit * permia)     !speed of light 
REAL*8 :: cc = 3*10**8   !speed of light 
INTEGER,DIMENSION(5) :: frequency
INTEGER :: I

REAL*8, PARAMETER :: permir_r_0 = 1.0
REAL*8 :: permit_r_1, permit_r_2 
REAL*8 :: permit_i_1, permit_i_2
REAL*8 :: loss_tan

INTEGER :: depth 
REAL*8 :: lamda_0 

!     +++++++++++++++++++++++++++++++++++++++++
!++++++RLFECTANCE AND TRANSMISSION COEFFICIENT++++++
!     +++++++++++++++++++++++++++++++++++++++++
REAL*8 :: T_01, T_10
REAL*8 :: R_12

!     +++++++++++++++++++++++++++
!++++++THE AMOUNT OF ATTENUATION++++++
!     +++++++++++++++++++++++++++
REAL*8 :: AT

!     ++++++++++++++++++++
!++++++RECEIVED INTENSITY++++++ 
REAL*8, DIMENSION(0:N,5) :: I_R


OPEN(UNIT=20, FILE = "/home/changwan/ATTENUATION/output_values.txt" &
     , STATUS = "REPLACE", ACTION = "WRITE")  

permit_r_1 = 16.0
permit_i_1 = 8.0
loss_tan = permit_i_1 / permit_r_1

!loss_tan = 0.1 !wet base
loss_tan = 0.31 !lossy layer


T_01 = 2 * sqrt(permit_i_1) / (1 + sqrt(permit_i_1))

T_10 = 2 / ( sqrt(permit_i_1) +1 )

R_12 = (sqrt(permit_r_1) - sqrt(permit_r_2) ) &
       / (sqrt(permit_r_1) + sqrt(permit_r_2))

frequency(1) = 15 * 10 ** (6)
frequency(2) = 20 * 10 ** (6)
frequency(3) = 35 * 10 ** (6)
frequency(4) = 40 * 10 ** (6)
frequency(5) = 80 * 10 ** (6)

!PRINT *, sqrt(permit*permia)

!PRINT *, "cc=", cc
DO I = 1, 5
   lamda_0 = cc / frequency(I)
   
   PRINT *, lamda_0

   DO depth = 0, N
!       depth = 0 
!       PRINT *, depth
       AT = 20.0* log10(exp(-2*pi*loss_tan*sqrt(permit_r_1)*depth/lamda_0))
  
!PRINT *, tan(theta)
!PRINT *, AT

      I_R(depth,I) =  AT

!       I_R(depth,I) = 10 * log10((ABS(T_01 * R_12 * T_10))**2) + AT
       I_R(depth,I) = 10 * log10((ABS(T_01 * R_12 * T_10))**2) + AT


   END DO
END DO

 DO depth = 0, N
   WRITE(20,*) (I_R (depth,I), I = 1,5)
 END DO
   
END PROGRAM
