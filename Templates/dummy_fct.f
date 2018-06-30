

      subroutine get_dummy_x1_x2(sjac, X1, R, pbeam1, pbeam2, stot, shat)
      implicit none
      include 'maxparticles.inc'
      include 'nexternal.inc'
      include 'run.inc'
c      include 'genps.inc'
      double precision sjac     ! jacobian. should be updated not reinit
      double precision X1(2)     ! bjorken X. output
      double precision R(2)     ! random value after grid transfrormation. between 0 and 1
      double precision pbeam1(0:3) ! momentum of the first beam (output)
      double precision pbeam2(0:3) ! momentum of the second beam (output)
      double precision stot        ! total energy
      double precision shat        ! output

      double precision pmass(nexternal)
      common/to_mass/  pmass

c     global variable to set (or not)
      double precision cm_rap
      logical set_cm_rap
      common/to_cm_rap/set_cm_rap,cm_rap

      double precision SMIN
      common/to_smin/ smin

      double precision xmin(2),xmax(2),m1,p0,p3
      double precision Emin,DMpdf     ! Dark Matter minimum energy and pdf

      logical firstcall
      data firstcall/.true./
      save firstcall,Emin

      call GENCMS2(Stot,X1(1),X1(2),R,SMIN,sjac)
      shat  =  x1(1)*x1(2)*stot
      pbeam1(:) = 0d0
      pbeam2(:) = 0d0
      pbeam1(0) = x1(1) * ebeam(1)
      pbeam1(3) = x1(1) * ebeam(1)
      pbeam2(0) = x1(2) * ebeam(2)
      pbeam2(3) = -x1(2) * ebeam(2)

      return
      end

      SUBROUTINE GENCMS2(S,X1,X2,X,SMIN,SJACOBI)
c--alternative -liq
      IMPLICIT NONE
      real*8 X1,X2,S,SMIN,SJACOBI,SJACOBI1,SJACOBI2
      real*8 X(2)
      real*8 felectron
      real*8 alpha
      real*8 xin
      real*8 PI
      real*8 feps
      IF (S .LT. SMIN) THEN
         PRINT*,'ERROR CMS ENERGY LESS THAN MINIMUM CMS ENERGY',S,SMIN
         RETURN
      ENDIF
      data xin/0.511d-3/ !electron mass in GeV
      alpha = 1d0/137d0
      data PI/3.1415926535897932384626433832795028841971693993751d0/
       feps=alpha/PI*(dlog(S/xin/xin)-1.d0) 
       X1=1.d0-(1.d0-X(1))**(1.d0/feps)
       X2=1.d0-(1.d0-X(2))**(1.d0/feps)
       if(X1.gt.1.0d0) then
           X1=1.0d0
       endif
       if(X2.gt.1.0d0) then
           X2=1.0d0
       endif
       SJACOBI1=felectron(X1,X(1),feps)
       SJACOBI2=felectron(X2,X(2),feps)
       SJACOBI=sjacobi*SJACOBI1*SJACOBI2
      END

      real*8 function felectron(x,xp,feps)
      real*8 x,xmin, zeta3
      real*8 xp 
      real*8 feps
      real*8 ftemp
      real*8 ft1
      double complex li2
      xmin = 0.00714053329734592839549879772019d0
      zeta3 = 1.20205690315959428539973816151d0

      ftemp=(-1.d0+feps)*dlog(1.d0-x)
      ftemp=dexp(ftemp)

      ft1=(1.d0+3.d0/4.d0*feps)
      ft1=ft1*(1d0-(1d0+x)*(1d0-x)/(1d0-xp)/2d0)
      ft1=ft1*(1d0+(27d0-8d0*PI*PI)/96d0*feps*feps)
      ft1=ft1*(1d0+(27d0-24d0*PI*PI+128d0*zeta3)/384d0*feps**3d0)

      if(x.gt.xmin.and.(1d0-x).gt.0d0) then
        
      ft1=ft1*(1d0-(
     $  (1d0+3d0*x*x)*dlog(x)/(1d0-xp)
     $     +(1d0-x)/(1d0-xp)*(4d0*(1d0+x)*dlog(1d0-xp)/feps+5d0+x)
     $ )*feps/8d0
     $)
      ft1=ft1*(1d0-(
     $  (1d0+x)*(1d0-x)*
     $   (6.0*dreal(li2(x))+12d0*dlog(1d0-xp)/feps
     $                   *dlog(1d0-xp)/feps-3d0*PI*PI)
     $      +1.5d0*(1d0+8d0*x+3d0*x*x)*dlog(x)
     $      +6d0*(x+5d0)*(1d0-x)*dlog(1d0-xp)/feps
     $      +12d0*(1d0+x*x)*dlog(x)*dlog(1d0-xp)/feps
     $      -(1d0+7d0*x*x)*dlog(x)**2/2.0d0
     $      +(39d0-24d0*x-15d0*x*x)/4.0d0
     $ )*feps**2/48d0/(1d0-xp)
     $)
      endif

      felectron= ft1
      end

      double complex function li2(x)
c--complex dilogarithm (spence-function) -liq
      implicit double precision (a-h,o-z)
      double complex x,y,cli2
      double precision zeta2,zeta3
      common/const/zeta2,zeta3
      logical first
      data first/.true./
      save first

      if (first) then
      first=.false.
      call bernini
      endif

      zero=1.d-8
      xr=dble(x)
      xi=dimag(x)
      r2=xr*xr+xi*xi
      li2=dcmplx(0d0,0d0)
      if(r2.le.zero)then
        li2=x+x**2/4.d0
        return
      endif
      rr=xr/r2
      if ((r2.eq.1.d0) .and. (xi.eq.0.d0)) then
        if (xr.eq.1.d0) then
          li2=dcmplx(zeta2)
        else
          li2=-dcmplx(zeta2/2.d0)
        endif
        return
      elseif ((r2.gt.1.d0) .and. (rr.gt.0.5d0)) then
        y=(x-1.d0)/x
        li2=cli2(y)+zeta2-cdlog(x)*cdlog(1.d0-x)+0.5d0*cdlog(x)**2
        return
      elseif ((r2.gt.1.d0) .and. (rr.le.0.5d0))then
        y=1.d0/x
        li2=-cli2(y)-zeta2-0.5d0*cdlog(-x)**2
        return
      elseif ((r2.le.1.d0) .and. (xr.gt.0.5d0)) then
        y=1.d0-x
        li2=-cli2(y)+zeta2-cdlog(x)*cdlog(1.d0-x)
       return
      elseif ((r2.le.1.d0) .and. (xr.le.0.5d0)) then
        y=x
        li2=cli2(y)
        return
      endif
      end
 
      double complex function cli2(x)
c--taylor-expansion for complex dilogarithm (spence-function)-qli
      implicit double precision (a-h,o-z)
      parameter(nber=18)
      double precision b2(nber) 
      double complex x,z
      common/bernoulli/b2

      n=nber-1
      z=-cdlog(1.d0-x)
      cli2=b2(nber)
      do 111 i=n,1,-1
        cli2=z*cli2+b2(i)
111   continue
      cli2=z**2*cli2+z
      return
      end
 
      double precision function facult(n)
c--double precision version of faculty-liq
      implicit double precision (a-h,o-z)
      facult=1.d0
      if(n.eq.0)return
      do 999 i=1,n
        facult=facult*dfloat(i)
999   continue
      return
      end
 
      subroutine bernini
c--initialization of coefficients for polylogarithms-liq
      implicit none
      real*8 PI
      integer nber,i
      parameter(nber=18)
      double precision b(nber),b2(nber),zeta2,zeta3,facult
      common/bernoulli/b2
      common/const/zeta2,zeta3
      data PI/3.14159265358979323846/
 
      b(1)=-1.d0/2.d0
      b(2)=1.d0/6.d0
      b(3)=0.d0
      b(4)=-1.d0/30.d0
      b(5)=0.d0
      b(6)=1.d0/42.d0
      b(7)=0.d0
      b(8)=-1.d0/30.d0
      b(9)=0.d0
      b(10)=5.d0/66.d0
      b(11)=0.d0
      b(12)=-691.d0/2730.d0
      b(13)=0.d0
      b(14)=7.d0/6.d0
      b(15)=0.d0
      b(16)=-3617.d0/510.d0
      b(17)=0.d0
      b(18)=43867.d0/798.d0
      zeta2=PI**2/6.d0
      zeta3=1.202056903159594d0
 
      do 995 i=1,nber
        b2(i)=b(i)/facult(i+1)
995   continue
 
      return
      end
