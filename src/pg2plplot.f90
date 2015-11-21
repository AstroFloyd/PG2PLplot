!> \file pg2plplot.f90  Contains pgplot-to-plplot bindings (i.e., call PLplot from PGplot commands)

! 
! LICENCE:
! 
! Copyright 2010-2013 AstroFloyd, joequant
!  
! This file is part of the PG2PLplot package.
!  
! This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
! by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this code (LICENCE).  If not, see
! <http://www.gnu.org/licenses/>.
! 
!
! PG2PLplot can be found on http://pg2plplot.sourceforge.net
! Some routines are taken from libSUFR, http://libsufr.sourceforge.net



!***********************************************************************************************************************************
!> \brief  PG2PLplot module

module PG2PLplot
  use plplot, only: plflt
  implicit none
  private plflt
  save
  
  !> Conversion factor for the character height
  real(plflt), parameter :: ch_fac = 0.35_plflt
  !logical, parameter :: compatibility_warnings = .false.  ! Don't warn
  logical, parameter :: compatibility_warnings = .true.   ! Do warn
  
  real(plflt) :: xcur=0., ycur=0.
  integer :: save_level = 0
  integer, parameter :: max_level = 20
  integer, parameter :: max_open = 100
  real(plflt), parameter :: mm_per_inch = 25.4
  integer :: save_ffamily(max_level)
  integer :: save_fstyle(max_level)
  integer :: save_fweight(max_level)
  integer :: save_lwidth(max_level)
  integer :: save_lstyle(max_level)
  integer :: save_color(max_level)
  integer :: cur_lwidth=1, cur_color=1, cur_lstyle=1
  integer :: devid=0
  logical :: is_init
  real(plflt) :: paper_width, paper_ratio
  
contains
  
  !*********************************************************************************************************************************
  subroutine do_init()
    implicit none
    if(.not. is_init) then
       call plinit()
       call plbop() 
       is_init = .true.
    end if
  end subroutine do_init
  !*********************************************************************************************************************************
  
end module PG2PLplot
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Set line style
!!
!! \param ls  Line style

subroutine pgsls(ls)
  use PG2PLplot, only : cur_lstyle
  implicit none
  integer, intent(in) :: ls
  integer :: ls1,ls2, styles(5)
  
  cur_lstyle = ls
  styles = (/1,4,5,2,6/)
  
  ls1 = ls   ! 1: solid, 2: dashes, 3: dash-dotted, 4: dotted, 5: dash-dot-dot-dot
  if(ls1.lt.1.or.ls1.gt.5) ls1 = 1
  
  
  ls2 = styles(ls1)  ! 1: solid, 2: short dashes, 3: long dashes, 4: long dashes, short gaps, 5: long-short dashes, 
  !                    6: long-short dashes, long-short gaps, 7: ?, 8: ?
  
  call pllsty(ls2)
  
  !print*,'pgsls: ',ls,ls1,ls2
  
end subroutine pgsls
!***********************************************************************************************************************************

!> \brief query line style
subroutine pgqls(ls)
  use PG2PLplot, only : cur_lstyle
  implicit none
  integer, intent(out) :: ls
  ls = cur_lstyle
end subroutine pgqls

!***********************************************************************************************************************************
!> \brief  Set line width
!!
!! \param lw  Line width

subroutine pgslw(lw)
  use plplot, only: plflt
  use PG2PLplot, only : cur_lwidth
  implicit none
  integer, intent(in) :: lw
  integer :: lw1
  real(kind=plflt) :: lw2
  
  cur_lwidth = lw
  lw1 = max(min(lw,201),1)
  lw2 = real(lw1 - 1)
  
  call plwidth(lw2)
  
  !print*,'pgslw: ',lw,lw1,lw2
  
end subroutine pgslw
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Query line width
!!
!! \param lw  Line width

subroutine pgqlw(lw)
  use PG2PLplot, only: cur_lwidth
  implicit none
  integer, intent(out) :: lw
  
  lw = max(1,cur_lwidth)
  
end subroutine pgqlw
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set character font
!!
!! \param cf  Character font

subroutine pgscf(cf)
  implicit none
  integer, intent(in) :: cf
  
  call plfont(cf)
  
end subroutine pgscf
!***********************************************************************************************************************************

!> \brief set query font
subroutine pgqcf(cf)
  implicit none
  integer, intent(out) :: cf
  integer :: family, style, weight
  call plgfont(family, style, weight)
  if(style .eq. 1) then
     cf = 3
     return
  else if(family .eq. 1) then
     cf = 2
     return
  else if(family .eq. 3) then
     cf = 4
     return
  end if
  cf = 1
end subroutine pgqcf

!> \brief
subroutine pgqinf(item, value, length)
  implicit none
  character, intent(in) :: item*(*)
  character, intent(out) :: value*(*)
  integer, intent(in) :: length
end subroutine pgqinf

!***********************************************************************************************************************************
!> \brief  Set colour index
!!
!! \param ci1  Colour index

subroutine pgsci(ci1)
  use PG2PLplot, only: cur_color
  implicit none
  integer, intent(in) :: ci1
  integer :: ci2,colours(0:15)
  cur_color = ci1
  ci2 = 15  ! White
  ci2 = ci1
  colours = (/0,15,1,3,9,7,13,2,8,12,4,11,10,5,7,7/)
  if(ci1.ge.0.and.ci1.le.15) ci2 = colours(ci1)
  
  call plcol0(ci2)
  
  !write(6,'(A,2I6)')'  pgsci: ',ci1,ci2
  
end subroutine pgsci
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set colour representation
!!
!! \param ci1  Colour index 
!! \param r    Red colour (0-1)
!! \param g    Green colour (0-1)
!! \param b    Blue colour (0-1)

subroutine pgscr(ci1, r,g,b)
  implicit none
  integer, intent(in) :: ci1
  real, intent(in) :: r,g,b
  integer :: ci2,ri,gi,bi,colours(0:15)
  
  ri = nint(r*255)
  gi = nint(g*255)
  bi = nint(b*255)
  
  colours = (/0,15,1,3,9,7,13,2,8,12,4,11,10,5,7,7/)
  ci2 = ci1
  if(ci1.ge.0.and.ci1.le.15) ci2 = colours(ci1)
  if(ci2.gt.15) call plscmap0n(256)  ! Allows indices 0-255
  
  call plscol0(ci2, ri,gi,bi)
  
  !write(6,'(A,2I6, 5x,3F10.3, 5x,3I6)')'  pgscr: ',ci1,ci2, r,g,b, ri,gi,bi
  
end subroutine pgscr
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Query colour representation
!!
!! \param  ci1  Colour index 
!! \retval r    Red colour (0-1)
!! \retval g    Green colour (0-1)
!! \retval b    Blue colour (0-1)

subroutine pgqcr(ci1, r,g,b)
  implicit none
  integer, intent(in) :: ci1
  real, intent(out) :: r,g,b
  integer :: ci2,ri,gi,bi,colours(0:15)
  
  ci2 = ci1
  colours = (/0,15,1,3,9,7,13,2,8,12,4,11,10,5,7,7/)
  if(ci1.ge.0.and.ci1.le.15) ci2 = colours(ci1)
  
  call plgcol0(ci2,ri,gi,bi)
  
  r = real(ri)/255.
  g = real(gi)/255.
  b = real(bi)/255.
  
  !write(6,'(A,2I6,5x,3I6,5x,3F10.3)')'  pgqcr: ',ci1,ci2, ri,gi,bi, r,g,b
  
end subroutine pgqcr
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set colour-index range for pggray and pgimag - dummy routine!
!!
!! \param  ci1  Lower colour index 
!! \param  ci2  Upper colour index 

subroutine pgscir(ci1,ci2)
  use PG2PLplot, only: compatibility_warnings
  implicit none
  integer, intent(in) :: ci1,ci2
  integer :: tmp
  integer, save :: warn
  
  tmp = ci1
  tmp = ci2
  tmp = tmp  ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgscir', '')
  warn = 123
  
end subroutine pgscir
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Query colour-index range for pggray and pgimag - dummy routine!
!!
!! \param  ci1  Lower colour index 
!! \param  ci2  Upper colour index 

subroutine pgqcir(ci1,ci2)
  use PG2PLplot, only: compatibility_warnings
  implicit none
  integer, intent(out) :: ci1,ci2
  integer, save :: warn
  
  ci1 = 0
  ci2 = 255
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgqcir', '')
  warn = 123
  
end subroutine pgqcir
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Set fill style
!!
!! \param fs  Fill style (1-4, no match for 2: outline)

subroutine pgsfs(fs)
  implicit none
  integer, intent(in) :: fs
  integer :: fs1,fs2, styles(4)
  
  fs1 = fs
  if(fs.lt.1.or.fs.gt.4) fs1 = 1
  
  ! fs1:  1: solid, 2: outline, 3-hatched, 4-cross-hatched
  ! fs2:  0: solid, 1: H lines, 2: V lines, 3: hatches 45d up, 4: hatches 45d down, 5: hatches 30d up, 6: hatches 30d down
  !       7: upright cross-hatces, 8: 45d cross-hatces
  
  styles = (/0,0,3,8/)
  
  fs2 = styles(fs1)
  
  call plpsty(fs2)  
  
  !print*,'pgsfs: ',fs1,fs2
  
end subroutine pgsfs
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set hash style
!!
!! \param ang  Angle of the lines (deg)
!! \param sep  Spacing (in % of view-surface size): >0!
!! \param ph   Phase of hatching.  Use e.g. 0.0 and 0.5 for double hatching - dummy variable: not used

subroutine pgshs(ang, sep, ph)
  implicit none
  real, intent(in) :: ang, sep, ph
  integer :: inc, del, tmp
  
  inc = nint(ang*10.)   ! Tenths of a degree
  del = nint(sep*1000)  ! Spacing in micrometers(!)
  tmp = nint(ph)
  tmp = tmp  ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  
  call plpat(1, inc, del)
  
end subroutine pgshs
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set character height
!!
!! \param ch  Character height

subroutine pgsch(ch)
  use plplot, only: plflt
  use PG2PLplot, only: ch_fac
  
  implicit none
  real, intent(in) :: ch
  real(kind=plflt) :: ch1,ch2
  
  ch1 = 0.0_plflt  ! 0: don't change
  ch2 = ch * ch_fac
  
  call plschr(ch1,ch2)
  
  !print*
  !print*,'pgsch1: ',ch,ch1,ch2
  !call plgchr(ch1,ch2)
  !print*,'pgsch2: ',ch,ch1,ch2
  
end subroutine pgsch
!***********************************************************************************************************************************

!>\brief

subroutine pgqcs(unit, xch, ych)
  use plplot, only: plflt
  use PG2PLplot, only: ch_fac, mm_per_inch
  
  implicit none
  integer, intent(in) :: unit
  real, intent(out) :: xch, ych
  real(kind=plflt) :: ch1,ch2
  real(kind=plflt) xp, yp, xleng, yleng, xoff, yoff
  call plgchr(ch1,ch2)
  if(unit.eq.2) then
     xch = ch2
     ych = ch2
  else if(unit .eq. 1) then
     xch = ch2 / mm_per_inch
     ych = ch2 / mm_per_inch
  else if(unit .eq. 0) then
     call plgpage(xp, yp, xleng, yleng, xoff, yoff)
     xch = ch2 / yleng
     ych = ch2 / yleng
  else
     print *, 'unknown unit in pgqcs', unit
  end if

end subroutine pgqcs

!***********************************************************************************************************************************
!> \brief  Query character height
!!
!! \param ch  Character height

subroutine pgqch(ch)
  use plplot, only: plflt
  use PG2PLplot, only: ch_fac
  
  implicit none
  real, intent(out) :: ch
  real(kind=plflt) :: ch1,ch2
  
  call plgchr(ch1,ch2)
  ch = real(ch2/(ch1*ch_fac))
  
  !print*,'pgqch: ',ch,ch1,ch2
  
end subroutine pgqch
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set arrow head - dummy routine!

subroutine pgsah(fs, angle, barb)
  use PG2PLplot, only: compatibility_warnings
  implicit none
  integer, intent(in) :: fs
  real, intent(in) :: angle, barb
  integer :: tmp
  integer, save :: warn
  
  tmp = fs
  tmp = nint(angle)
  tmp = nint(barb)
  tmp = tmp  ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgsah', '')
  warn = 123
  
end subroutine pgsah
!***********************************************************************************************************************************







!***********************************************************************************************************************************
!> \brief  Draw a line
!!
!! \param n  Number of points
!! \param x1 X-values of points
!! \param y1 Y-values of points

subroutine pgline(n,x1,y1)
  use plplot, only: plflt, plline
  
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  
  x2 = x1
  y2 = y1
  
  call plline(x2,y2)
  
  !write(6,'(A,99ES16.9)')'  pgline1:  ',x1(1:min(n,9)),y1(1:min(n,9))
  !write(6,'(A,99ES16.9)')'  pgline2:  ',x2(1:min(n,9)),y2(1:min(n,9))
  
end subroutine pgline
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Draw an arrow - only a line is drawn, arrows not supported in PLplot!
!!
!! \param x1  X-value of start point
!! \param y1  Y-value of start point
!! \param x2  X-value of end point
!! \param y2  Y-value of end point

subroutine pgarro(x1,y1, x2,y2)
  use plplot, only: plflt, plline, plpoin
  
  implicit none
  real, intent(in) :: x1,x2, y1,y2
  real(kind=plflt) :: x(2),y(2)
  
  x = (/x1,x2/)
  y = (/y1,y2/)
  
  call plline(x,y)
  call plpoin((/x(2)/),(/y(2)/),2)
  
  !print*,'pgarro: ',x1,x2,y1,y2,'  ',x,y
  
end subroutine pgarro
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Draw points
!!
!! \param n  Number of points
!! \param x1 X-values of points
!! \param y1 Y-values of points
!! \param s  Plot symbol to use

subroutine pgpoint(n,x1,y1,s)
  use plplot, only: plflt, plpoin
  
  implicit none
  integer, intent(in) :: n,s
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  
  x2 = x1
  y2 = y1
  
  call plpoin(x2,y2,s)
  !call plsym(1,x2,y2,143)  ! Produces Hershey -> many letters, etc
  
end subroutine pgpoint
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Draw a polygone
!!
!! \param n  Number of points
!! \param x1 X-values of points
!! \param y1 Y-values of points

subroutine pgpoly(n,x1,y1)
  use plplot, only: plflt, plfill
  
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  
  x2 = x1
  y2 = y1
  
  call plfill(x2,y2)
  
  !print*,'pgpoly: ',n,x1(1),x1(n),y1(1),y1(n)
  
end subroutine pgpoly
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Draw a rectangle
!!
!! \param x1  Lower x value
!! \param x2  Upper x value
!! \param y1  Lower y value
!! \param y2  Upper y value

subroutine pgrect(x1,x2,y1,y2)
  use plplot, only: plflt, plfill
  
  implicit none
  real, intent(in) :: x1,x2,y1,y2
  real(kind=plflt) :: x(4),y(4)
  
  x = (/x1,x1,x2,x2/)
  y = (/y1,y2,y2,y1/)
  
  call plfill(x,y)
  
end subroutine pgrect
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Draw a circle
!!
!! \param xc X-value of centre
!! \param yc Y-value of centre
!! \param r  Radius

subroutine pgcirc(xc, yc, r)
  use plplot, only: plflt, plfill
  
  implicit none
  real, intent(in) :: xc, yc, r
  
  integer, parameter :: n=100
  integer :: i
  real(kind=plflt) :: x(n),y(n),twopi
  
  twopi = 8.*atan(1.)
  
  do i=1,n
     x(i) = xc + r * cos(twopi/real(n-1))
     y(i) = yc + r * sin(twopi/real(n-1))
  end do
  
  call plfill(x,y)
  
end subroutine pgcirc
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Make a contour plot
!!
!! \param arr  Data array
!! \param nx   Dimension 1 of data array
!! \param ny   Dimension 2 of data array
!! \param ix1  Start index range in dimension 1 to use from data array
!! \param ix2  End index range in dimension 1 to use from data array
!! \param iy1  Start index range in dimension 2 to use from data array
!! \param iy2  End index range in dimension 2 to use from data array
!! \param c    Array with heights to draw contours for
!! \param nc   Dimension of c
!! \param tr   Affine transformation elements

subroutine pgcont(arr, nx,ny, ix1,ix2, iy1,iy2, c, nc, tr)
  use plplot, only: plflt, plcont
  
  implicit none
  integer, intent(in) :: nx,ny, ix1,ix2, iy1,iy2, nc
  real, intent(in) :: arr(nx,ny), c(*), tr(6)
  real(kind=plflt) :: arr1(nx,ny), clevel(nc), tr1(6)
  
  arr1 = arr
  clevel = c(1:nc)
  tr1 = tr
  
  call plcont(arr1, ix1,ix2, iy1,iy2, clevel, tr1)
  
end subroutine pgcont
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Shade a region (between contours/heights) -  dummy routine!
!!
!! \param arr  Data array
!! \param nx   Dimension 1 of data array
!! \param ny   Dimension 2 of data array
!! \param ix1  Start index range in dimension 1 to use from data array
!! \param ix2  End index range in dimension 1 to use from data array
!! \param iy1  Start index range in dimension 2 to use from data array
!! \param iy2  End index range in dimension 2 to use from data array
!! \param c1   Lower limit in height/contour to fill
!! \param c2   Upper limit in height/contour to fill
!! \param tr   Affine transformation elements

subroutine pgconf(arr, nx,ny, ix1,ix2, iy1,iy2, c1, c2, tr)
  use PG2PLplot, only: compatibility_warnings
  implicit none
  integer, intent(in) :: nx,ny, ix1,ix2, iy1,iy2
  real, intent(in) :: arr(nx,ny), c1,c2, tr(6)
  !real(kind=plflt) :: arr1(nx,ny), clevel(nc), tr1(6)
  integer :: tmp
  integer, save :: warn
  
  tmp = nint(arr(1,1))
  tmp = nx
  tmp = ny
  tmp = ix1
  tmp = ix2
  tmp = iy1
  tmp = iy2
  tmp = nint(c1)
  tmp = nint(c2)
  tmp = nint(tr(1))
  tmp = tmp  ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  
  
  !arr1 = arr
  !clevel = c(1:nc)
  !tr1 = tr
  !
  !call plshade1(arr1, ix1,ix2, iy1,iy2, clevel, tr1)
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgconf', '')
  warn = 123
  
end subroutine pgconf
!***********************************************************************************************************************************













!***********************************************************************************************************************************
!> \brief  Print text with arbitrary angle and justification
!!
!! \param x1     X-coordinate of text
!! \param y1     Y-coordinate of text
!! \param ang    Angle
!! \param just1  Justification
!! \param text   Text to print
!!
!! \note  Angle only correct for 0,90,180,270deg or square viewport

subroutine pgptxt(x1,y1,ang,just1,text)
  use plplot, only: plflt, plptex
  
  implicit none
  real, intent(in) :: x1,y1,ang,just1
  character, intent(in) :: text*(*)
  real :: d2r
  real(kind=plflt) :: x2,y2,just2,dx,dy,xmin,xmax,ymin,ymax
  character :: text1*(len(text))
  
  d2r = atan(1.)/45.
  call plgvpw(xmin,xmax,ymin,ymax)
  
  ! Convert angle -> dy/dx
  dx = (xmax-xmin)*0.1
  
  if(abs(ang).lt.1.e-5) then                       ! ang ~ 0 deg
     dy =  0.0
  else if(abs(mod(ang-90.,180.)).lt.1.e-5) then    ! ang = +/-90deg
     dx =  0.0
     dy = -1.0                                     ! ang = -90deg
     if(abs(mod(ang-90.,360.)).lt.1.e-5) dy = 1.   ! ang = +90deg
  else
     dx = 1.0
     dy = dx*tan(ang*d2r) * (ymax-ymin)/(xmax-xmin)
     if(ang.gt.90. .and. ang.lt.270.  .or. ang.lt.-90. .and. ang.gt.-270.) then
        dx = -dx
        dy = -dy
     end if
  end if
  
  x2 = x1
  y2 = y1
  just2 = just1
  
  text1 = text
  call pg2pltext(text1)
  
  call plptex(x2,y2,dx,dy,just2,trim(text1))
  
  !write(6,'(A,4F10.3,A)')'  pgptxt: ',x1,y1,ang,just1,trim(text)
  !write(6,'(A,5F10.3,A)')'  pgptxt: ',x2,y2,dx,dy,just2,trim(text1)
  
end subroutine pgptxt
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Non-standard alias for pgptxt()
!!
!! \param x     X-coordinate of text
!! \param y     Y-coordinate of text
!! \param ang   Angle
!! \param just  Justification
!! \param text  Text to print
!!
!! \note  Angle only right for 0,90,180,270deg or square viewport

subroutine pgptext(x,y,ang,just,text)
  implicit none
  real, intent(in) :: x,y,ang,just
  character, intent(in) :: text*(*)
  
  call pgptxt(x,y,ang,just,text)
  
end subroutine pgptext
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Print text with default angle and justification
!!
!! \param x1     X-coordinate of text
!! \param y1     Y-coordinate of text
!! \param text   Text to print

subroutine pgtext(x1,y1,text)
  use plplot, only: plflt, plptex
  
  implicit none
  real, intent(in) :: x1,y1
  character, intent(in) :: text*(*)
  real(kind=plflt) :: x2,y2,just,dx,dy
  character :: text1*(len(text))
  
  ! Convert angle=0deg -> dy/dx
  dx = 1.
  dy = 0.
  just = 0.  !Left-adjusted
  
  x2 = x1
  y2 = y1
  
  text1 = text
  call pg2pltext(text1)
  call plptex(x2,y2,dx,dy,just,text1)
  
end subroutine pgtext
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Print text in the margin
!!
!! \param side   Side to print text on ('L','R','T','B')
!! \param disp1  Distance from axis
!! \param pos1   Position along axis
!! \param just1  Justification
!! \param text   Text to print

subroutine pgmtxt(side, disp1, pos1, just1, text)
  use plplot, only: plflt, plmtex
  
  implicit none
  real, intent(in) :: disp1,pos1,just1
  real(kind=plflt) :: disp2,pos2,just2
  character, intent(in) :: side*(*)
  character, intent(in) :: text*(*)
  character :: text1*(len(text))
  
  disp2 = disp1
  pos2  = pos1
  just2 = just1
  
  !write(6,'(2A,2(3F10.3,5x),A)')'  pgmtxt: ',trim(side),disp1,pos1,just1,disp2,pos2,just2,trim(text)
  
  text1 = text
  call pg2pltext(text1)
  call plmtex(side, disp2, pos2, just2, text1)
  
end subroutine pgmtxt
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Alias for pgmtxt()
!!
!! \param side   Side to print text on ('L','R','T','B')
!! \param disp   
!! \param pos    Position
!! \param just   Justification
!! \param text   Text to print

subroutine pgmtext(side, disp, pos, just, text)
  implicit none
  real, intent(in) :: disp,pos,just
  character, intent(in) :: side*(*)
  character, intent(in) :: text*(*)
  character :: text1*(len(text))
  
  text1 = text
  call pg2pltext(text1)
  call pgmtxt(side, disp, pos, just, text1)
  
end subroutine pgmtext
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief Interface for pglab
!!
!! \param xlbl    Label for the horizontal axis
!! \param ylbl    Label for the vertical axis
!! \param toplbl  Plot title

subroutine pglab(xlbl, ylbl, toplbl)
  implicit none
  character, intent(in) :: xlbl*(*), ylbl*(*), toplbl*(*)
  call pgbbuf
  call pgmtxt('T', 2.0, 0.5, 0.5, TOPLBL)
  call pgmtxt('B', 3.2, 0.5, 0.5, XLBL)
  call pgmtxt('L', 2.2, 0.5, 0.5, YLBL)
  call pgebuf
end subroutine pglab
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief Alias for pglab
!!
!! \param xlbl    Label for the horizontal axis
!! \param ylbl    Label for the vertical axis
!! \param toplbl  Plot title

subroutine pglabel(xlbl, ylbl, toplbl)
  implicit none
  character, intent(in) :: xlbl*(*), ylbl*(*), toplbl*(*)
  call pglab(xlbl, ylbl, toplbl)
end subroutine pglabel
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Start a new plot
!!
!! \param pgdev  PGplot device
!!
!! This function creates a new stream for each xwin request, but uses the same stream (0) for each file stream request.
!! Xwin streams are treated differently, since we want bufferring for smooth animations whereas the other streams are non-buffered.

function pgopen(pgdev)
  use plplot, only: plspause, plsfnam, plsdev
  use plplot, only: plmkstrm, plsetopt
  use PG2PLplot, only : is_init, devid
  implicit none
  integer :: pgopen
  character, intent(in) :: pgdev*(*)
  
  integer :: cur_stream=0, check_error
  character :: pldev*(99),filename*(99)
  
  filename = 'plot_temp.png'
  
  call pg2pldev(pgdev, pldev,filename)  ! Extract pldev and filename from pgdev
  call plmkstrm(cur_stream)  
  call plssub(1, 1)
  call plsdev(trim(pldev))
  if(trim(pldev).ne.'xwin') then
     if(check_error(trim(filename)).ne. 0) then
        pgopen = -1
        return
     end if
     call plsfnam(trim(filename))       ! Set output file name
  else
     call plsetopt("db", "")
  end if
  
  !call plscolbg(255,255,255)           ! Set background colour to white
  call plfontld(1)                      ! Load extended character set(?)
  call plspause(.false.)                ! Pause at plend()
  
  pgopen = cur_stream + 1
  devid = pgopen
  is_init = .false.
end function pgopen
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Inquire current device identifier
!!
!! \retval  Device identifier

subroutine pgqid(id)
  use PG2PLplot, only : devid
  implicit none
  integer, intent(out) :: id
  id = devid
end subroutine pgqid
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Begin a new plot
!!
!! \param i      Display ID
!! \param pgdev  PGplot device
!! \param nx     Number of frames in the x-direction
!! \param ny     Number of frames in the y-direction
!! 
!! This is a bit simpler than pgopen(), since I don't need to create a new stream for each request.

subroutine pgbegin(i,pgdev,nx,ny)
  use plplot, only: plspause, plsfnam, plsetopt, plssub, plsdev
  use PG2PLplot, only : is_init
  implicit none
  integer, intent(in) :: i,nx,ny
  character, intent(in) :: pgdev*(*)
  integer :: i1, check_error
  character :: pldev*(99),filename*(99)
  
  ! These are ignored by pgbegin. Can't be self, since calling arguments are likely constants
  i1 = i
  i1 = nx
  i1 = ny
  i1 = i1
  
  call pg2pldev(pgdev, pldev,filename)  ! Extract pldev and filename from pgdev
  
  if(trim(pldev).ne.'xwin') then
     if(check_error(trim(filename)).ne.0) return
     call plsfnam(trim(filename))       ! Set output file name
  else
     call plsetopt("db", "")
  end if
  
  call plfontld(1)                      ! Load extended character set(?)
  call plssub(1, 1)
  call plsdev(trim(pldev))  
  is_init = .false.
  call plspause(.false.)                ! Pause at plend()
end subroutine pgbegin
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Alias for pgbegin
!!
!! \param i      Display ID
!! \param pgdev  PGplot device
!! \param nx     Number of frames in the x-direction
!! \param ny     Number of frames in the y-direction

subroutine pgbeg(i, pgdev, nx, ny)
  implicit none
  integer, intent(in) :: i,nx,ny
  character, intent(in) :: pgdev*(*)
  call pgbegin(i, pgdev, nx, ny)
end subroutine pgbeg
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  End a plot

subroutine pgend()
  implicit none
  
  call plflush()
  call plend1()  
end subroutine pgend
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set paper size
!!
!! \param width  Paper width
!! \param ratio  Paper aspect ratio

subroutine pgpap(width,ratio)
  use PG2PLplot, only: paper_width, paper_ratio, do_init
  use plplot, only : plflt
  implicit none
  real, intent(in) :: width, ratio
  integer :: xlen,ylen,xoff,yoff
  real(kind=plflt) :: xp,yp
  
  xp = 300.  ! DPI
  yp = 300.
  paper_width = width
  paper_ratio = ratio
  
  xlen = nint(paper_width*xp)
  ylen = nint(paper_width*xp*paper_ratio)
  
  xoff = 0  ! Offset
  yoff = 0
  
  call plspage(xp,yp,xlen,ylen,xoff,yoff)  ! Must be called before plinit()!
  call do_init()
  
end subroutine pgpap
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set view port
!!
!! \param xl1  Left side of the x-axis
!! \param xr1  Right side of the x-axis
!! \param yb1  Bottom side of the y-axis
!! \param yt1  Top side of the y-axis

subroutine pgsvp(xl1,xr1,yb1,yt1)
  use plplot, only: plflt
  
  implicit none
  real, intent(in) :: xl1,xr1,yb1,yt1
  real(kind=plflt) :: xl2,xr2,yb2,yt2
  
  xl2 = xl1
  xr2 = xr1
  yb2 = yb1
  yt2 = yt1
  !write(6,'(A,2(4F10.3,5x))')'  pgsvp: ',xl1,xr1,yb1,yt1,xl2,xr2,yb2,yt2
  
  call plvpor(xl2,xr2,yb2,yt2)
  
end subroutine pgsvp
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Set window
!!
!! \param xmin1  Left
!! \param xmax1  Right
!! \param ymin1  Top
!! \param ymax1  Bottom

subroutine pgswin(xmin1,xmax1,ymin1,ymax1)
  use plplot, only: plflt
  
  implicit none
  real, intent(in) :: xmin1,xmax1,ymin1,ymax1
  real(kind=plflt) :: xmin2,xmax2,ymin2,ymax2
  
  xmin2 = xmin1
  xmax2 = xmax1
  ymin2 = ymin1
  ymax2 = ymax1
  !write(6,'(A,2(4F10.3,5x))')'  pgswin: ',xmin1,xmax1,ymin1,ymax1,xmin2,xmax2,ymin2,ymax2
  
  call plwind(xmin2,xmax2,ymin2,ymax2)
  
end subroutine pgswin
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief alias for pgswin
!!
!! \param xmin1  Left
!! \param xmax1  Right
!! \param ymin1  Top
!! \param ymax1  Bottom

subroutine pgwindow(xmin1,xmax1,ymin1,ymax1)
  use plplot, only: plflt
  
  implicit none
  real, intent(in) :: xmin1,xmax1,ymin1,ymax1
  real(kind=plflt) :: xmin2,xmax2,ymin2,ymax2
  call plwind(xmin2,xmax2,ymin2,ymax2)
end subroutine pgwindow
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Subdivide view surface into panels
!!
!! \param nxsub   Number of subticks on the x-axis
!! \param nysub   Number of subticks on the y-axis

subroutine pgsubp(nxsub, nysub)
  implicit none
  integer, intent(in) :: nxsub,nysub
  
  call plssub(nxsub, nysub)
  
end subroutine pgsubp
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Advance to the next (sub-)page

subroutine pgpage()
  use PG2PLplot, only: do_init
  implicit none
  
  call pladv(0)
  call do_init()
  
end subroutine pgpage
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> Alias for pgpage
subroutine pgadvance()
  call pgpage()
end subroutine pgadvance
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Start buffering output - dummy routine!

subroutine pgbbuf()
  use PG2PLplot, only: compatibility_warnings, do_init
  implicit none
  integer, save :: warn
  
  call do_init()
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgbbuf', '')
  warn = 123
  
end subroutine pgbbuf
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  End buffering output - dummy routine!

subroutine pgebuf()
  use PG2PLplot, only: compatibility_warnings
  implicit none
  integer, save :: warn
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgebuf', '')
  warn = 123
  
end subroutine pgebuf
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Draw a box (+axes) around a plot
!!
!! \param xopt    Options for the x-axis
!! \param xtick1  Distance between ticks on the x-axis
!! \param nxsub   Number of subticks on the x-axis
!! \param yopt    Options for the y-axis
!! \param ytick1  Distance between ticks on the y-axis
!! \param nysub   Number of subticks on the y-axis

subroutine pgbox(xopt, xtick1, nxsub, yopt, ytick1, nysub)
  use plplot, only: plflt, plbox
  
  implicit none
  integer, intent(in) :: nxsub,nysub
  real, intent(in) :: xtick1,ytick1
  character, intent(in) :: xopt*(*),yopt*(*)
  real(kind=plflt) :: xtick2,ytick2
  
  xtick2 = xtick1
  ytick2 = ytick1
  !write(6,'(A,2(2F10.3,5x))')'  pgbox: ',xtick1,ytick1,xtick2,ytick2
  
  call plbox(xopt, xtick2, nxsub, yopt, ytick2, nysub)
  
end subroutine pgbox
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Draw a single tick mark, optionally with label - no PLplot routine found, using an ad-hoc routine
!!
!! \param x1      world coordinates of one endpoint of the axis
!! \param y1      world coordinates of one endpoint of the axis
!! \param x2      world coordinates of the other endpoint of the axis
!! \param y2      world coordinates of the other endpoint of the axis
!!
!! \param pos     draw the tick mark at fraction pos (0<=pos<=1) along the line from (X1,Y1) to (X2,Y2)
!! \param tikl    length of tick mark drawn to the left or bottom of the axis - drawing ticks outside the box may not be supported
!! \param tikr    length of major tick marks drawn to the right or top of the axis - drawing outside the box may not be supported
!!
!! \param disp    displacement of label text from the axis
!! \param orient  orientation of label text, in degrees
!! \param lbl     text of label (may be blank)

subroutine pgtick(x1, y1, x2, y2, pos, tikl, tikr,  disp, orient, lbl)
  use plplot, only: plflt, plmtex
  
  implicit none
  real, intent(in) :: x1, y1, x2, y2, pos, tikl, tikr, disp, orient
  character, intent(in) :: lbl*(*)
  
  real :: x,y,dx,dy, dpx,dpy, reldiff
  real(plflt) :: p_xmin,p_xmax,p_ymin,p_ymax, plx1,plx2,ply1,ply2, tlen, disp1,pos1,just
  character :: lbl1*(len(lbl))
  logical :: seq,deq
  
  disp1 = disp
  x = orient  ! Unused
  x = x       ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  lbl1 = lbl
  
  dx = x2 - x1
  dy = y2 - y1
  x  = x1 + pos*dx
  y  = y1 + pos*dy
  
  call plgvpw(p_xmin, p_xmax, p_ymin, p_ymax)
  
  dpx = real(abs(p_xmax-p_xmin))
  dpy = real(abs(p_ymax-p_ymin))
  
  
  tlen = 0.03  ! default size of a tickmark
  pos1 = pos
  just = 0.5
  
  if(reldiff(y1,y2) .le. epsilon(y1)*10 .or. (seq(y1,0.) .and. seq(y2,0.))) then                ! Want horizontal axis
     
     plx1 = x
     plx2 = x
     ply1 = y
     
     ! Plot ticks below the axis:
     if(tikl.gt.tiny(tikl)) then
        ply2 = y - dpy * tikl * tlen
        call pljoin(plx1,ply1,plx2,ply2)
     end if
     
     ! Plot ticks above the axis:
     if(tikr.gt.tiny(tikr)) then
        ply2 = y + dpy * tikr * tlen
        call pljoin(plx1,ply1,plx2,ply2)
     end if
     
     ! Print labels:
     if(abs(reldiff(y,real(p_ymin))) .le. epsilon(y)*10 .or. (seq(y,0.) .and. deq(p_ymin,0.0_plflt))) then
        call plmtex('B', disp1, pos1, just, trim(lbl1))                                         ! Print label below the bottom axis
     else
        call plmtex('T', disp1, pos1, just, trim(lbl1))                                         ! Print label above the top axis
     end if
     
  else if(reldiff(x1,x2) .le. epsilon(x1)*10 .or. (seq(x1,0.) .and. seq(x2,0.))) then           ! Want vertical axis
     
     ply1 = y
     ply2 = y
     plx1 = x
     
     ! Plot ticks to the left of the axis:
     if(tikl.gt.tiny(tikl)) then
        plx2 = x - dpx * tikl * tlen
        call pljoin(plx1,ply1, plx2,ply2)
     end if
     
     ! Plot ticks to the right of the axis:
     if(tikr.gt.tiny(tikr)) then
        plx2 = x + dpx * tikr * tlen
        call pljoin(plx1,ply1, plx2,ply2)
     end if
     
     ! Print labels:
     if(abs(reldiff(x,real(p_xmin))) .le. epsilon(x)*10 .or. (seq(x,0.) .and. deq(p_xmin,0.0_plflt))) then
        call plmtex('L', disp1, pos1, just, trim(lbl1))                           ! Print label to the left of the left-hand axis
     else
        call plmtex('R', disp1, pos1, just, trim(lbl1))                           ! Print label to the right of the right-hand axis
     end if
     
  else
     write(0, '(A)') "PG2PLplot, pgtick(): x1!=x2 and y1!=y2 - I don't know which axis you want to use..."
     print*,'x:',x1,x2,reldiff(x1,x2)
     print*,'y:',y1,y2,reldiff(y1,y2)
  end if
  
end subroutine pgtick
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Read data from screen - no PLplot equivalent (yet) - dummy routine!
!!
!! \todo No plplot routine found yet - using dummy

subroutine pgolin(maxpt, npt, x, y, symbol)
  use PG2PLplot, only: compatibility_warnings
  
  implicit none
  integer, intent(in) :: maxpt,symbol
  integer, intent(out) :: npt
  real, intent(out) :: x(maxpt),y(maxpt)
  
  integer :: symbol1
  integer, save :: warn
  
  npt = maxpt
  x = 0.d0
  y = 0.d0
  symbol1 = symbol
  symbol1 = symbol1  ! Avoid 'variable is set but not used' warnings from compiler for dummy variable
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgolin', '')
  warn = 123
  
end subroutine pgolin
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Erase screen

subroutine pgeras()
  use plplot, only: plclear
  implicit none
  call plclear()
end subroutine pgeras
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Replace the PGPlot escape character '\' with the PLplot escape character '#'
!!
!! \param string  Text string to convert

subroutine pg2pltext(string)
  implicit none
  character, intent(inout) :: string*(*)
  
  !print*,trim(string)
  
  call replace_substring(string, '\', '#')        ! Replace the PGPlot escape character \ with the PLplot escape character # '
  call replace_substring(string, '#(0248)', '#(2246)')  ! \approx -> \sim
  call replace_substring(string, '#(062',   '#(212')    ! alpha - gamma
  call replace_substring(string, '#(063',   '#(213')    ! delta - nu
  call replace_substring(string, '#(064',   '#(214')    ! xi - psi
  call replace_substring(string, '#(0650',  '#(2150')   ! omega
  call replace_substring(string, '#(0685)', '#(2185)')  ! (var)theta
  
  !print*,trim(string)
  
end subroutine pg2pltext
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Converts PGplot device ID to PLplot device ID + file name
!!
!! \param  pgdev     PGplot device ID (includes filename: 'file.name/dev')
!! \retval pldev     PLplot device ID ('dev')
!! \retval filename  PLplot file name ('file.name')
!!
!!
!! \note Possible values for pldev:
!!  - xwin:      
!!  - wxwidgets: 
!!  - xcairo:    
!!  - qtwidget:  
!!
!!  - png:       no anti-aliasing in lines
!!  - wxpng:     no extended characters
!!  - pngcairo:  
!!  - pngqt:     

subroutine pg2pldev(pgdev, pldev,filename)
  implicit none
  character, intent(in) :: pgdev*(*)
  character, intent(out) :: pldev*(*), filename*(*)
  integer :: i
  
  
  i = index(pgdev, '/', back=.true.)
  
  filename = ' '
  if(i.ne.1) filename = pgdev(1:i-1)
  
  pldev = ' '
  pldev = pgdev(i+1:)
  
  ! Change PGPlot ps to PLplot ps:
  call replace_substring(pldev,'vcps','psc')
  call replace_substring(pldev,'cvps','psc')
  call replace_substring(pldev,'cps','psc')
  call replace_substring(pldev,'vps','ps')
  
  ! Change PGplot ppm output tot png:
  call replace_substring(pldev,'ppm','png')
  call replace_substring(filename,'ppm','png')
  
  ! Use pngqt rather than png:
  !call replace_substring(pldev,'png','pngqt')
  
  ! Use pngcairo rather than png, since I get segfaults if outputing to both X11 and png which pulls in pngqt (joequant):
  if(pldev .eq. 'png') pldev = 'pngcairo'
end subroutine pg2pldev
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Select output stream
!!
!! \param  pgdev stream number

subroutine pgslct(pgdev)
  use PG2PLplot, only: do_init
  use plplot, only : plspause, plgdev
  
  implicit none
  integer, intent(in) :: pgdev
  character :: pldev*(99)
  
  call do_init()
  call plgdev(pldev)
  if(trim(pldev).eq.'xwin') call pleop()
  call plsstrm(pgdev-1)
  
end subroutine pgslct
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Save parameters

subroutine pgsave()
  use PG2PLplot, only: save_level, max_level, cur_color
  use PG2PLplot, only: save_ffamily, save_fstyle, save_fweight
  use PG2PLplot, only: save_lwidth, save_color, save_lstyle
  use PG2PLplot, only: cur_lstyle, cur_lwidth
  use plplot, only : plgfont
  implicit none
  
  save_level = save_level + 1
  if(save_level.gt.max_level) then
     write(0,'(/,A,/)') '***  PG2PLplot WARNING: too many save calls in pgsave()'
     return
  end if
  
  call plgfont(save_ffamily(save_level), save_fstyle(save_level), save_fweight(save_level))
  save_lwidth(save_level) = cur_lwidth
  save_color(save_level) = cur_color
  save_lstyle(save_level) = cur_lstyle
end subroutine pgsave
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Unsave parameters

subroutine pgunsa()
  use PG2PLplot, only: save_level, max_level
  use PG2PLplot, only: save_ffamily, save_fstyle, save_fweight
  use PG2PLplot, only: save_lwidth, save_color, save_lstyle
  use plplot, only : plsfont
  implicit none
  
  if(save_level.eq.0) then
     write(0,'(/,A,/)') '***  PG2PLplot WARNING: no save call in stack in pgunsa()'
     return
  end if
  
  if(save_level.gt.max_level) then
     write(0,'(/,A,/)') '***  PG2PLplot WARNING: unsave greater than stack in pgunsa()'
     save_level = save_level - 1
     return
  end if
  
  call plsfont(save_ffamily(save_level), save_fstyle(save_level), save_fweight(save_level))
  call pgslw(save_lwidth(save_level))
  call pgsci(save_color(save_level))
  call pgsls(save_lstyle(save_level))
  save_level = save_level - 1
end subroutine pgunsa
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Move pen to location - for use with pgdraw() only!
!!
!! \param x  Horizontal location
!! \param y  Vertical location

subroutine pgmove(x, y)
  use PG2PLplot, only: xcur, ycur
  implicit none
  real, intent(in) :: x, y
  
  xcur = x
  ycur = y
  
end subroutine pgmove
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Draw line to location
!!
!! \param x  Horizontal location
!! \param y  Vertical location

subroutine pgdraw(x, y)
  use PG2PLplot, only: xcur, ycur
  use plplot, only: pljoin, plflt
  implicit none
  real, intent(in) :: x, y
  real(plflt) :: x1, y1
  
  x1 = x
  y1 = y
  
  call pljoin(xcur, ycur, x1, y1)
  call pgmove(x, y)
  
end subroutine pgdraw
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Draw a point
!!
!! \param n      Number of data points to draw
!! \param x      Horizontal locations
!! \param y      Vertical locations
!! \param ncode  Plot symbol

subroutine pgpt(n, x, y, ncode)
  use plplot, only: plpoin, plflt, plsym
  
  implicit none
  integer, intent(in) :: n, ncode
  real(plflt), intent(in), dimension(n) :: x, y
  
  integer :: code
  
  
  if(ncode == -3) then
     code = 7
  else if(ncode == -4) then
     code = 6
  else if(ncode < 0) then
     code = 22
  else
     code = ncode
  end if
  
  call plsym(x, y, code)
  
end subroutine pgpt
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Draw a single point
!!
!! \param x      Horizontal location
!! \param y      Vertical location
!! \param ncode  Plot symbol

subroutine pgpt1(x, y, ncode)
  use plplot, only: plpoin, plflt
  implicit none
  integer, intent(in) :: ncode
  real(plflt), intent(in) :: x, y
  real(plflt), dimension(1) :: xin, yin
  
  xin(1) = x
  yin(1) = y
  
  call pgpt(1, xin, yin, ncode)
  
end subroutine pgpt1
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Close stream

subroutine pgclos()
  implicit none
  call plend1()
end subroutine pgclos
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Get cursor location - dummy routine!
!!

subroutine pgband(mode, posn, xref, yref, x, y, ch)
  use PG2PLplot, only: compatibility_warnings
  
  implicit none
  integer, intent(in) :: mode, posn
  real, intent(in) :: xref, yref
  real, intent(out) :: x, y
  character, intent(out) :: ch*(*)
  
  integer, save :: warn
  integer :: i1
  real :: r1
  
  
  i1 = mode
  i1 = posn
  i1 = i1  ! Remove 'unused variable error from g95'
  
  r1 = xref
  r1 = yref
  r1 = r1  ! Remove 'unused variable error from g95'
  
  x = 0.0
  y = 0.0
  ch = char(0)
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgband', '')
  warn = 123
  
end subroutine pgband
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Get colour range - dummy routine!

subroutine pgqcol(c1, c2)
  use PG2PLplot, only: compatibility_warnings
  
  implicit none
  integer, intent(out) :: c1, c2
  integer, save :: warn
  
  
  c1 = 0
  c2 = 255
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgqcir','using a default colour range')
  warn = 123
  
end subroutine pgqcol
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Get current colour index - dummy routine!
!!
!! \retval ci  Colour index

subroutine pgqci(ci)
  use PG2PLplot, only: compatibility_warnings
  
  implicit none
  integer, intent(out):: ci
  integer, save :: warn
  
  ci = 0
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgqcir','using a default colour index')
  warn = 123
  
end subroutine pgqci
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Get viewport size
!!
!! \param units  specify the units of the output parameters:
!!                - 0 : normalized device coordinates
!!                - 1 : inches
!!                - 2 : millimeters
!!                - 3 : pixels
!!
!! \retval x1  Left
!! \retval x2  Right
!! \retval y1  Bottom
!! \retval y2  Top

subroutine pgqvp(units, x1, x2, y1, y2)
  use plplot, only: plflt
  use PG2PLplot, only : mm_per_inch
  implicit none
  integer, intent(in):: units
  real, intent(out):: x1, x2, y1, y2
  real(kind=plflt) x1d, x2d, y1d, y2d
  real(kind=plflt) xp, yp, xleng, yleng, xoff, yoff
  call plgvpd(x1d, x2d, y1d, y2d)
  if(units .ne. 0) then
     call plgpage(xp, yp, xleng, yleng, xoff, yoff)
     x1 = (x1d * xleng - xoff) 
     x2 = (x2d * xleng - xoff)
     y1 = (y1d * yleng - yoff)
     y2 = (y2d * yleng - yoff) 
     if(units .eq. 3) then
        return
     else if(units .eq. 2) then
        x1 = x1 / xp * mm_per_inch
        x2 = x2 / xp* mm_per_inch
        y1 = y1 / yp * mm_per_inch
        y2 = y2 / yp* mm_per_inch
        return
     else if(units .eq. 1) then
        x1 = x1 / xp
        x2 = x2 / xp
        y1 = y1 / yp
        y2 = y2 / yp
        return
     end if
  else
     print *, "unknown units in pgqvp", units
  end if
  x1=x1d
  x2=x2d
  y1=y1d
  y2=y2d
end subroutine pgqvp

!> \brief get view surface
subroutine pgqvsz(units, x1, x2, y1, y2)
  use plplot, only: plflt
  use PG2PLplot, only : mm_per_inch
  implicit none
  integer, intent(in):: units
  real, intent(out):: x1, x2, y1, y2
  real(kind=plflt) xp, yp, xleng, yleng, xoff, yoff
  x1 = 0.0
  y1 = 0.0

  if(units .eq. 0) then
     x2 = 1.0
     y2 = 1.0
     return
  end if

  call plgpage(xp, yp, xleng, yleng, xoff, yoff)
  if(units .eq. 3) then
     x2 = xleng
     y2 = yleng
  else if(units .eq. 2) then
     x2 = xleng / xp* mm_per_inch
     y2 = yleng / yp* mm_per_inch
  else if(units .eq. 1) then
     x2 = x2 / xp
     y2 = y2 / yp
     return
  else
     print *, 'undefined units in pgqvsz'
     return
  end if
end subroutine pgqvsz
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Print user name and date in plot - dummy routine!

subroutine pgiden()
  use PG2PLplot, only: compatibility_warnings
  
  implicit none
  integer, save :: warn
  
  if(.not.compatibility_warnings) warn = 123  ! Don't warn about compatibility between PGPlot and PLplot
  if(warn.ne.123) call warn_dummy_routine('pgiden','ignoring')
  warn = 123
  
end subroutine pgiden
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief get window size
!!
!! \retval x1  Left
!! \retval x2  Right
!! \retval y1  Bottom
!! \retval y2  Top

subroutine pgqwin(x1, x2, y1, y2)
  use plplot, only: plflt
  implicit none
  real, intent(out):: x1, x2, y1, y2
  real(kind=plflt):: xmin, xmax, ymin, ymax
  call plgspa(xmin, xmax, ymin, ymax)
  x1 = xmin
  x2 = xmax
  y1 = ymin
  y2 = ymax
end subroutine pgqwin
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Flush - dummy routine

subroutine pgupdt()
  return
end subroutine pgupdt
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Search and replace occurences of a substring in a string, taken from libSUFR
!!
!! \param string   Original string to replace in
!! \param str_in   Search string
!! \param str_out  Replacement string

subroutine replace_substring(string, str_in, str_out)
  implicit none
  character, intent(inout) :: string*(*)
  character, intent(in) :: str_in*(*),str_out*(*)
  integer :: is,l
  
  l = len_trim(str_in)
  is = huge(is)
  do
     is = index(string, str_in, back=.false.)
     if(is.le.0) exit
     string = string(1:is-1)//trim(str_out)//trim(string(is+l:))
  end do
  
end subroutine replace_substring
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Inquire fill-area style - dummy routine
!!
!! \retval fs  The current fill-area style

subroutine pgqfs(fs)
  implicit none
  integer, intent(out) :: fs
  fs = 1
end subroutine pgqfs
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set text background color index - dummy routine
!!
!! \param b  Background color index

subroutine pgstbg(b)
  implicit none
  integer, intent(in) :: b
end subroutine pgstbg
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Query text background color index - dummy routine
!!
!! \param b  Background color index

subroutine pgqtbg(b)
  implicit none
  integer, intent(out) :: b
end subroutine pgqtbg
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Control new page prompting - dummy routine
!!
!! \param prompt  Set prompt state to ON for interactive devices

subroutine pgask(prompt)
  implicit none
  logical, intent(in) :: prompt
end subroutine pgask
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set window and viewport and draw labeled frame
!!
!! \param xmin  Left
!! \param xmax  Right
!! \param ymin  Top (?)
!! \param ymax  Bottom (?)
!! \param just  JUST=1: scales of x and y axes are equal, otherwise independent
!! \param axis  Controls the plotting of axes, tick marks, etc

subroutine pgenv(xmin, xmax, ymin, ymax, just, axis)
  use plplot, only: plflt
  implicit none
  real, intent(in) :: xmin, xmax, ymin, ymax
  integer, intent(in) :: just, axis
  real(kind=plflt) :: xminp, xmaxp, yminp, ymaxp
  xminp = xmin
  xmaxp = xmax
  yminp = ymin
  ymaxp = ymax
  call plenv(xminp, xmaxp, yminp, ymaxp, just, axis)
end subroutine pgenv
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Plot a histogram of binned data
!!
!! \param nbin    Number of bins
!! \param x       Abscissae of the bins
!! \param data    Data values of bins
!! \param center  If .TRUE., the X values denote the center of the bin, else the lower edge

subroutine pgbin(nbin, x, data, center)
  use plplot, only: plflt, plbin
  implicit none
  integer, intent(in) :: nbin
  real, intent(in) :: x(*), data(*)
  logical, intent(in) :: center
  real(kind=plflt) :: xp(nbin), datap(nbin)
  integer :: opt
  xp = x(1:nbin)
  datap = data(1:nbin)
  if(center) opt = 1
  call plbin(xp, datap, opt)
end subroutine pgbin
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Vertical error bar
!!
!! \param n     Number of error bars to plot
!! \param x     World horizontal coordinates of the data
!! \param ymax  World vertical coordinates of top end of the error bars
!! \param ymin  World vertical coordinates of bottom end of the error bars
!! \param t     Length of terminals to be drawn at the ends of the error bar

subroutine pgerry(n, x, ymax, ymin, t)
  use plplot, only: plflt, plerry
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: x(*), ymin(*), ymax(*), t
  real(kind=plflt) :: xp(n), yminp(n), ymaxp(n)

  xp = x(1:n)
  yminp = ymin(1:n)
  ymaxp = ymax(1:n)
  call plerry(xp, yminp, ymaxp)
end subroutine pgerry
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Set viewport (inches)
!!
!! \param xleft   Horizontal coordinate of left-hand edge of viewport, in inches from left edge of view surface
!! \param xright  Horizontal coordinate of right-hand edge of viewport, in inches from left edge of view surface
!! \param ybot    Vertical coordinate of bottom edge of viewport, in inches from bottom of view surface
!! \param ytop    Vertical coordinate of top edge of viewport, in inches from bottom of view surface

subroutine pgvsiz(xleft, xright, ybot, ytop)
  use plplot, only: plflt
  implicit none
  real, intent(in) :: xleft, xright, ybot, ytop
  real(kind=plflt) :: x1p, x2p, y1p, y2p
  x1p=xleft
  x2p=xright
  y1p=ybot
  y2p=ytop
  call plsvpa(x1p, x2p, y1p, y2p)
end subroutine pgvsiz
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Non-standard alias for PGVSIZ
!!
!! \param xleft   Horizontal coordinate of left-hand edge of viewport, in inches from left edge of view surface
!! \param xright  Horizontal coordinate of right-hand edge of viewport, in inches from left edge of view surface
!! \param ybot    Vertical coordinate of bottom edge of viewport, in inches from bottom of view surface
!! \param ytop    Vertical coordinate of top edge of viewport, in inches from bottom of view surface

subroutine pgvsize(xleft, xright, ybot, ytop)
  implicit none
  real, intent(in) :: xleft, xright, ybot, ytop
  call pgvsiz(xleft, xright, ybot, ytop)
end subroutine pgvsize
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Find length of a string in a variety of units
!!
!! \param units   0: normalized device coordinates, 1: in, 2: mm, 3: absolute device coordinates (dots), 4: world coordinates,
!!                5: fraction of the current viewport size
!! \param string  String of interest
!! \param xl      Length of string in horizontal direction
!! \param yl      Length of string in vertical direction

subroutine pglen(units, string, xl, yl)
  implicit none
  character, intent(in) :: string *(*)
  integer, intent(in) :: units
  real, intent(out):: xl, yl
  print *, "pglen not implemented"
  xl = 1.0
  yl = 1.0
end subroutine pglen
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  Find bounding box of text string
!!
!! \param x      World x-coordinate
!! \param y      World y-coordinate
!! \param angle  Angle between baseline and horizontal (degrees)
!! \param fjust  Horizontal justification
!! \param text   Text string that would be written
!!
!! \retval xbox  Horizontal limits of the bounding box
!! \retval ybox  Vertical limits of the bounding box


subroutine pgqtxt(x, y, angle, fjust, text, xbox, ybox)
  implicit none
  character, intent(in) :: text *(*)
  integer, intent(in) :: x, y, angle, fjust
  real, intent(out):: xbox(4), ybox(4)
  print *, "pgqtxt not implemented"
  xbox(1) = 0.0
  xbox(2) = 1.0
  xbox(3) = 0.0
  xbox(4) = 1.0
  ybox(1) = 0.0
  ybox(2) = 1.0
  ybox(3) = 0.0
  ybox(4) = 1.0
end subroutine pgqtxt
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Return the relative difference between two numbers: dx/\<x\>  -  taken from libSUFR, turned into single precision
!!
!! \param x1  First number
!! \param x2  Second number

function reldiff(x1,x2)
  implicit none
  real, intent(in) :: x1,x2
  real :: reldiff, xsum,xdiff
  
  xsum  = x1+x2
  xdiff = x2-x1
  
  if(abs(xsum).gt.tiny(xsum)) then
     reldiff = xdiff / (xsum*0.5)
  else                     ! Can't divide by zero
     if(abs(xdiff).gt.tiny(xdiff)) then
        reldiff = xdiff
     else
        reldiff = 1.0  ! 0/0
     end if
  end if
  
end function reldiff
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Check whether opening a file gives an error
!!
!! \param fname  File name

function check_error(fname)
  implicit none
  character, intent(in) :: fname*(*)
  integer :: err, check_error
  
  open(9999, file=trim(fname), err=450, iostat=err)
  close(9999)
  check_error = 0
  return
  
450 continue
  check_error = err
  
end function check_error
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Print a warning when no (proper) PLplot equivalent is defined and a dummy routine is used instead
!!
!! \param routine  Name of the undefined routine
!! \param message  Message to be postponed

subroutine warn_dummy_routine(routine, message)
  implicit none
  character, intent(in) :: routine*(*),message*(*)
  
  write(0,'(/,A)') '***  PG2PLplot WARNING: no PLplot equivalent was found for the PGplot routine '//trim(routine)//'() '// &
       trim(message)//'  ***'
  
end subroutine warn_dummy_routine
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Test whether two double-precision variables are equal to better than twice the machine precision, taken from libSUFR
!!
!! \param x1  First number
!! \param x2  Second number

function deq(x1,x2)
  use plplot, only: plflt
  
  implicit none
  real(plflt), intent(in) :: x1,x2
  real(plflt) :: eps
  logical :: deq
  
  eps = 2*tiny(x1)
  if(abs(x1-x2).le.eps) then
     deq = .true.
  else
     deq = .false.
  end if
  
end function deq
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Test whether two single-precision variables are equal to better than twice the machine precision, taken from libSUFR
!!
!! \param x1  First number
!! \param x2  Second number

function seq(x1,x2)
  implicit none
  real, intent(in) :: x1,x2
  real :: eps
  logical :: seq
  
  eps = 2*tiny(x1)
  if(abs(x1-x2).le.eps) then
     seq = .true.
  else
     seq = .false.
  end if
  
end function seq
!***********************************************************************************************************************************


