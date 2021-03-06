c=======================================================================
      subroutine newpag (xmin, xmax, ymin, ymax, xlab, ylab, title,
     .  echx,echy)
c
c  version : 04.07.97 19:39
c
* Start a new plot
      implicit none
c
c  donnees
c  xmin, xmax: valeurs minimum et maximum de x
c  ymin, ymax:    "       "    "     "    "  y
c  xlab:       titre de l'axe x
c  ylab:         "   "     "  y
c  title:      titre du graphique
c  echx, echy: echelles des axes x et y respectivement.
c
      real xmin,xmax,ymin,ymax
      character*(*) xlab,ylab,title,echx,echy
c
c  variables en common
c
#include <PERIM.F>
c
c  variables locales
      integer ll
      real zx(2),zy(2),xmi,xma,ymi,yma,etalx,etaly
c
c  procedures
      INTRINSIC ABS,MAX
      EXTERNAL AGSETF,AGSETI,AGSETC,AGSTUP,WTSTR,AGBACK ! graphy plot function belong to NCAR
     .  ,EZXY,FRAME,AGCURV,getset,cadre
! AGSETF: belong to NACR: http://ngwww.ucar.edu/supplements/autograph/#OVERVIEWDrawingAGraph


c  preliminary definition of the frame
      call cadre
C  DEFINE THE ARRAYS CONTAINING THE EXTREMA
      ZX(1)=XMIN
      ZX(2)=XMAX
      ZY(1)=YMIN
      ZY(2)=YMAX
* Specify the box ! similar with axis([x1a,x1b,x2a,x2b]) in matlab
      CALL AGSETF('GRID/LEFT.',x1a) ! specify the real value
      CALL AGSETF('GRID/RIGHT.',x1b)
      CALL AGSETF('GRID/BOTTOM.',x2a)
      CALL AGSETF('GRID/TOP.',x2b)
* Turn off clipping
      CALL AGSETI('WINDOW.',0) ! same as agsetf, specify the integer value
* Write title ! title() in matlab
      CALL AGSETC('LABEL/NAME.','T') ! write the title in character type
      CALL AGSETI('LINE/NUMBER.',100) ! INTEGER
      CALL AGSETF('LINE/CHARACTER.',.05) ! float
      CALL AGSETC('LINE/TEXT.',TITLE) ! character
* Write x label
      CALL AGSETC('LABEL/NAME.','B') ! xlabel in matlab
      CALL AGSETI('LINE/NUMBER.',-100)
      CALL AGSETF('LINE/CHARACTER.',.05)
      CALL AGSETC('LINE/TEXT.',XLAB)
* Write y label
      CALL AGSETC('LABEL/NAME.','L') ! ylabel() in matlab
      CALL AGSETF('LABEL/ANGLE.',90.)
      CALL AGSETI('LINE/NUMBER.',100)
      CALL AGSETF('LINE/CHARACTER.',.05)
      CALL AGSETC('LINE/TEXT.',YLAB)
C  SET THE BACKGROUND TYPE TO PERIMETER
C     CALL AGSETI('BACKGROUND.',1)
* Set linear (default) or logarithmic scaling
      IF(ECHX.EQ.'LOG' .or. echx.eq.'log') THEN ! semilogx in matlab
        CALL AGSETI('X/LOGARITHMIC.',1)
      ELSE
        CALL AGSETI('X/LOGARITHMIC.',0)
      ENDIF
      IF(ECHY.EQ.'LOG' .or.echy.eq.'log') THEN ! semilogy
        CALL AGSETI('Y/LOGARITHMIC.',1)
      ELSE
        CALL AGSETI('Y/LOGARITHMIC.',0)
      ENDIF
C  ALLOW BOTH SHRINKAGE AND ROTATION OF NUMERIC LABELS
c     CALL AGSETF('AXIS/LEFT/CONTROL.',4)
c     CALL AGSETF('AXIS/BOTTOM/CONTROL.',4)
* Scientific format for labels
      IF(((echx.ne.'LOG' .and. echx.ne.'log')
     .  .and. MAX(ABS(XMIN),ABS(XMAX)).LE.999.
     .  .AND. MAX(ABS(XMIN),ABS(XMAX)).GE..001)
     .  .or.
     .  (echx.eq.'LOG' .and. xmax.le.999. .and. xmin.ge..001)) THEN
        CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.) ! if varlue<0.001 .or. value>1000 ,use the scientfic format for label
      ELSE
        CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',1.) ! type
      ENDIF
      CALL AGSETF('AXIS/BOTTOM/NUMERIC/ANGLE/1ST.',0.)
      CALL AGSETF('AXIS/BOTTOM/NUMERIC/ANGLE/2ND.',90.)
      CALL AGSETF('AXIS/BOTTOM/NUMERIC/WIDTH/MAN.',.05)
      CALL AGSETF('AXIS/BOTTOM/NUMERIC/WIDTH/EXP.',.05) ! width
      IF(((echy.ne.'LOG' .and. echy.ne.'log') ! setting for y coordiante, ditto
     .  .and. MAX(ABS(YMIN),ABS(YMAX)).LE.999.
     .  .AND. MAX(ABS(YMIN),ABS(YMAX)).GE..001)
     .  .or.
     .  (echy.eq.'LOG' .and. ymax.le.999. .and. ymin.ge..001)) THEN
        CALL AGSETF('AXIS/LEFT/NUMERIC/TYPE.',3.)
      ELSE
        CALL AGSETF('AXIS/LEFT/NUMERIC/TYPE.',1.)
      ENDIF
      CALL AGSETF('AXIS/LEFT/NUMERIC/ANGLE/1ST.',0.)
      CALL AGSETF('AXIS/LEFT/NUMERIC/ANGLE/2ND.',90.)
      CALL AGSETF('AXIS/LEFT/NUMERIC/WIDTH/MAN.',.05)
      CALL AGSETF('AXIS/LEFT/NUMERIC/WIDTH/EXP.',.05)
C  SET THE NUMBER OF MAJOR TICK MARKS
      CALL AGSETF('AXIS/BOTTOM/MAJOR/COUNT.',2.) ! marksize in matlab
      CALL AGSETF('AXIS/LEFT/MAJOR/COUNT.',2.) ! tickminor in matlab
C     CALL AGSETF('AXIS/BOTTOM/MAJOR/BASE.',.25*(x1b-x1a))
C     CALL AGSETF('AXIS/BOTTOM/MAJOR/TYPE.',1.)
C     CALL AGSETF('AXIS/LEFT/MAJOR/BASE.',.25*(x1b-x1a))
C     CALL AGSETF('AXIS/LEFT/MAJOR/TYPE.',1.)
* Draw grid
      CALL AGSTUP(ZX,1,0,2,1,ZY,1,0,2,1) ! grid on in matlab
c  redefine the box for unit aspect ratio when possible ! axis equal .or. square .or. auto
      call getset(x1a,x1b,x2a,x2b,xmi,xma,ymi,yma,ll) ! belong to NCAR, check the argument to last call set
      if(ll.eq.1) then
        etalx=xma-xmi ! x(end)-x(1)
        etaly=yma-ymi ! y(end)-y(1)
        if(etalx.lt.etaly .and. etalx.ge.etaly/2) then
          x1b=x1a+etalx/etaly*(x2b-x2a) ! scaling the x coordinate
          CALL AGSETF('GRID/LEFT.',x1a)
          CALL AGSETF('GRID/RIGHT.',x1b)
          CALL AGSETF('GRID/BOTTOM.',x2a)
          CALL AGSETF('GRID/TOP.',x2b)
          CALL AGSTUP(ZX,1,0,2,1,ZY,1,0,2,1) ! autograph setup
        elseif(etaly.lt.etalx .and. etaly.ge.etalx/2) then
          x2b=x2a+etaly/etalx*(x1b-x1a)
          CALL AGSETF('GRID/LEFT.',x1a)
          CALL AGSETF('GRID/RIGHT.',x1b)
          CALL AGSETF('GRID/BOTTOM.',x2a)
          CALL AGSETF('GRID/TOP.',x2b)
          CALL AGSTUP(ZX,1,0,2,1,ZY,1,0,2,1)
        endif
      endif
      CALL AGBACK ! draw the BACKGROUND
* Clipping
      CALL AGSETI('WINDOW.',1) ! integer
      return
      end
