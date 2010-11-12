!> \file pg2plplot.f90  Contains pgplot-to-plplot bindings (i.e., call PLplot from PGplot commands)


!***********************************************************************************************************************************
!> \brief  
subroutine pgsls(i)
  implicit none
  integer, intent(in) :: i
  call pllsty(i)
end subroutine pgsls
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgslw(i)
  implicit none
  integer, intent(in) :: i
  call plwid(i)
end subroutine pgslw
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgscf(i)
  implicit none
  integer, intent(in) :: i
  call plfont(i)
end subroutine pgscf
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgsci(c1)
  implicit none
  integer, intent(in) :: c1
  integer :: c2,colours(0:15)
  
  c2 = 15 !White
  colours = (/0,15,1,3,9,7,13,2,8,12,4,11,10,5,7,7/)
  if(c1.ge.0.and.c1.le.15) c2 = colours(c1)
  !write(6,'(A,2I6)')'  pgsci: ',c1,c2
  call plcol0(c2)
end subroutine pgsci
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgscr(c1,r,g,b)
  implicit none
  integer, intent(in) :: c1
  real, intent(in) :: r,g,b
  integer :: c2,ri,gi,bi,colours(0:15)
  
  ri = nint(r*255)
  gi = nint(g*255)
  bi = nint(b*255)
  
  colours = (/0,15,1,3,9,7,13,2,8,12,4,11,10,5,7,7/)
  c2 = c1
  if(c1.ge.0.and.c1.le.15) c2 = colours(c1)
  call plscol0(c2,ri,gi,bi)
  !write(6,'(A,2I6,3F10.3)')'  pgscr: ',c1,c2,r,g,b
end subroutine pgscr
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgsfs(i)
  implicit none
  integer, intent(in) :: i
  call plpsty(i)
end subroutine pgsfs
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgsch(x1)
  use plplot
  implicit none
  real, intent(in) :: x1
  real(kind=plflt) :: x2
  x2 = x1*0.7
  call plschr(0.0_plflt,x2)
end subroutine pgsch
!***********************************************************************************************************************************






!***********************************************************************************************************************************
!> \brief  
subroutine pgline(n,x1,y1)
  use plplot
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  x2 = x1
  y2 = y1
  !write(6,'(A,99F6.2)')'  pgline:  ',x1(21:29),y1(21:29)
  !write(6,'(A,99F6.2)')'  pgline:  ',x2(21:29),y2(21:29)
  call plline(x2,y2)
end subroutine pgline
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgpoint(n,x1,y1,s)
  use plplot
  implicit none
  integer, intent(in) :: n,s
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  x2 = x1
  y2 = y1
  call plpoin(x2,y2,s)
end subroutine pgpoint
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgpoly(n,x1,y1)
  use plplot
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: x1(n),y1(n)
  real(kind=plflt) :: x2(n),y2(n)
  x2 = x1
  y2 = y1
  call plfill(x2,y2)
end subroutine pgpoly
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgrect(x1,x2,y1,y2)
  use plplot
  implicit none
  real, intent(in) :: x1,x2,y1,y2
  real(kind=plflt) :: x(4),y(4)
  x = (/x1,x1,x2,x2/)
  y = (/y1,y2,y2,y1/)
  call plfill(x,y)
end subroutine pgrect
!***********************************************************************************************************************************





!***********************************************************************************************************************************
!> \brief  
subroutine pgptxt(x1,y1,ang,just1,text)  !Angle only right for 0,90,180,270deg or square viewport
  use plplot
  implicit none
  real, intent(in) :: x1,y1,ang,just1
  real :: d2r
  real(kind=plflt) :: x2,y2,just2,dx,dy,xmin,xmax,ymin,ymax
  character, intent(in) :: text*(*)
  
  d2r = atan(1.)/45.
  call plgvpw(xmin,xmax,ymin,ymax)
  !print*,xmin,xmax,ymin,ymax
  
  !Convert angle -> dy/dx
  dx = xmax-xmin
  !print*,ang,abs(mod(ang-90.,180.)),tan(ang*d2r)
  if(abs(mod(ang-90.,180.)).lt.1.e-5) then        !ang = +/-90deg
     dx = 0.
     dy = -1.                                      !ang = -90deg
     if(abs(mod(ang-90.,360.)).lt.1.e-5) dy = 1.  !ang = +90deg
  else
     dy = dx*tan(ang*d2r) * (ymax-ymin) !/(xmax-xmin)
     if(ang.gt.90..and.ang.lt.270. .or. ang.lt.-90..and.ang.gt.-270.) then
        dx = -dx
        dy = -dy
     end if
  end if
  
  x2 = x1
  y2 = y1
  just2 = just1
  
  call plptex(x2,y2,dx,dy,just2,text)
  !write(6,'(A,4F10.3,A)')'  pgptext: ',x1,y1,ang,just1,trim(text)
  call pg2pltext(text)
  !write(6,'(A,5F10.3,A)')'  pgptext: ',x2,y2,dx,dy,just2,trim(text)
end subroutine pgptxt
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgtext(x1,y1,text)
  use plplot
  implicit none
  real, intent(in) :: x1,y1
  real(kind=plflt) :: x2,y2,just,dx,dy
  character, intent(in) :: text*(*)
  
  !Convert angle=0deg -> dy/dx
  dx = 1.
  dy = 0.
  just = 0.  !Left-adjusted
  
  x2 = x1
  y2 = y1
  call pg2pltext(text)
  call plptex(x2,y2,dx,dy,just,text)
end subroutine pgtext
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgmtxt(side, disp1, pos1, just1, text)
  use plplot
  implicit none
  real, intent(in) :: disp1,pos1,just1
  real(kind=plflt) :: disp2,pos2,just2
  character, intent(in) :: side*(*),text*(*)
  disp2 = disp1
  pos2 = pos1
  just2 = just1
  
  !write(6,'(2A,2(3F10.3,5x),A)')'  pgmtxt: ',trim(side),disp1,pos1,just1,disp2,pos2,just2,trim(text)
  
  call pg2pltext(text)
  call plmtex(side, disp2, pos2, just2, text)  
end subroutine pgmtxt
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!> \brief  
subroutine pgmtext(side, disp, pos, just, text)
  implicit none
  real, intent(in) :: disp,pos,just
  character, intent(in) :: side*(*),text*(*)
  !call pg2pltext(text)
  call pgmtxt(side, disp, pos, just, text)  
end subroutine pgmtext
!***********************************************************************************************************************************





!***********************************************************************************************************************************
!> \brief  
!> 
!! \todo Need to convert pgdev -> pldev + filename as in pgbegin()
function pgopen(pgdev)
  use plplot
  implicit none
  integer :: pgopen
  character, intent(in) :: pgdev*(*)
  character :: pgdev1*(len_trim(pgdev))
  character :: pldev*99,filename*99
  
  pgdev1 = pgdev
  
  !pldev = 'xwin'
  !pldev = 'wxwidgets'
  !pldev = 'xcairo'
  !pldev = 'qtwidget'
  
  pldev = 'png'       !No anti-aliasing in lines
  !pldev = 'wxpng'     !No extended characters
  !pldev = 'pngcairo'
  !pldev = 'pngqt'
  
  filename = 'plot_temp.png'
  
  !call plsdev(trim(pldev))
  !call plinit()
  if(pldev.ne.'xwin') call plsfnam(trim(filename))         !Set output file name
  call plfontld(1)                     !Load extended character set(?)
  call plspause(.true.)                !Pause at pgend()
  
  call plstart(trim(pldev),1,1)        !Initialise plplot with pldev with 1x1 subpages
  call pladv(0)                        !Advance to first (sub)page
  
  pgopen = 1
end function pgopen
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgbegin(i,pgdev,nx,ny)
  use plplot
  implicit none
  integer, intent(in) :: i,nx,ny
  character, intent(in) :: pgdev*(*)
  integer :: i1
  character :: pldev*99,filename*99
  
  i1=i !Is ignored by pgbegin, can't be self, since calling argument is likely a constant
  i1 = i1
  !Need to convert pgdev -> pldev + filename as in pgbegin()
  pldev = trim(pgdev)
  !pldev = 'xwin'
  
  !pldev = 'xwin'
  !pldev = 'wxwidgets'
  !pldev = 'xcairo'
  !pldev = 'qtwidget'
  
  pldev = 'png'       !No anti-aliasing in lines
  !pldev = 'wxpng'     !No extended characters
  !pldev = 'pngcairo'
  !pldev = 'pngqt'
  
  filename = 'plot_temp.png'
  
  call plsfnam(trim(filename))         !Set output file name
  call plfontld(1)                     !Load extended character set(?)
  call plspause(.true.)                !Pause at pgend()
  
  call plstart(trim(pldev),nx,ny)
  call pladv(0)  
  call plspause(.false.)
  
end subroutine pgbegin
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgend()
  implicit none
  call plend()
end subroutine pgend
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgpap(width,ratio)
  use plplot
  implicit none
  real, intent(in) :: width,ratio
  integer :: xlen,ylen,xoff,yoff
  real(kind=plflt) :: xp,yp
  
  xp = 300.  !DPI
  yp = 300.  
  xlen = nint(width*xp)
  ylen = nint(width*xp*ratio)
  xoff = 0  !Offset
  yoff = 0
  
  !call plspage(xp,yp,xlen,ylen,xoff,yoff)  !CHECK: must be called before plinit()
end subroutine pgpap
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pgsvp(xl1,xr1,yb1,yt1)
  use plplot
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
!> \brief  
subroutine pgswin(xmin1,xmax1,ymin1,ymax1)
  use plplot
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
!> \brief  
subroutine pgbox(xopt, xtick1, nxsub, yopt, ytick1, nysub)
  use plplot
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
!> \brief  
!> 
!! \todo No plplot routine found yet - using dummy
subroutine pgolin(maxpt, npt, x, y, symbol)
  use plplot
  implicit none
  integer, intent(in) :: maxpt,npt,symbol
  real, intent(in) :: x(:),y(:)
  integer :: maxpt1,npt1,symbol1
  real :: x1(size(x)),y1(size(y))
  
  maxpt1 = maxpt
  npt1 = npt
  x1 = x
  y1 = y
  symbol1 = symbol
  
end subroutine pgolin
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  
subroutine pg2pltext(string)  !Replace the PGPlot escape character '\' with the PLplot escape character '#'
  implicit none
  character, intent(inout) :: string*(*)
  integer :: i,n
  
  n = len_trim(string)
  do i=1,n
     if(string(i:i).eq.'\') string(i:i) = '#' !'
  end do
end subroutine pg2pltext
!***********************************************************************************************************************************
