
MODULE MinuitAPI

!  USE MODMinuit

  IMPLICIT NONE

  Interface Fit
     Module Procedure Fit1D, FitMD, Fit1DCorr, FitMDCorr
  End Interface

  Private Fit1D, FitMD, Fit1DCorr, FitMDCorr

CONTAINS

! ***********************************************
! *
  Subroutine Fit1D(X, Y, Yerr, Func, Coef, Chisqr, logfile)
! *
! ***********************************************

    Real (kind=8), Intent (in) :: X(:), Y(:), Yerr(:)
    Real (kind=8), Intent (inout) :: Coef(:)
    Real (kind=8), Intent (out) :: Chisqr
    Character (len=*), Optional :: logfile
    
    Integer :: Idummy, I, Ierr, Ifoo
    Real (kind=8) :: foo
    Character (len=1) :: cfoo

    Interface 
       Function Func(X, C)
         Real (kind=8), Intent (in) :: X
         Real (kind=8), Intent (in) :: C(:)
         Real (kind=8) :: Func
       End Function Func
    End Interface

    If (Present(logfile)) Then
       Open (unit=69, File=Trim(logfile))
    Else
       Open (unit=69, File='minuit.log')
    End If
    CALL MnInit(5,69,69)
    CALL Mncomd(Fm,"set pri -1",Ierr, fdummy)
    CALL Mncomd(Fm,"set now",Ierr, Fdummy)
    CALL Mncomd(Fm,"set str 2",Ierr, Fdummy)
    
    Do I = 1, Size(Coef)
       CALL MnParm(I, 'X', Coef(I), 1.0D0, 0.0D0, 0.0D0, Ierr)
    End Do

    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    CALL Mncomd(Fm,"seek",Ierr,Fdummy)
    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    
!    Output the Chisqr, and parameter value
    CALL MNstat(Chisqr, foo, foo, Ifoo, Ifoo, Ierr)
    Do I = 1, Size(Coef)
       CALL MnPout(I, cfoo, Coef(I), foo, foo, foo, Ierr)
    End Do

    Close(69)

    Return
  Contains

    Subroutine Fm(Npar,Grad,Fval,Xval,Iflag,Fdummy)
      
      Integer, Intent (in) :: Npar, Iflag
      Real (kind=8), Intent (out) :: Fval
      Real (kind=8), Intent (in) :: Xval(*), Grad(*), Fdummy
      
      Integer :: I
      Real (kind=8) :: C(Size(Coef))

      Do I = 1, Size(Coef)
         C(I) = Xval(I)
      End Do

      Fval = 0.0D0
      Do I = 1, Size(X)
         FVal = Fval + (Y(I) - Func(X(I),C))**2 / Yerr(I)**2 
      End Do

      Return
    End Subroutine Fm

    Subroutine fdummy()

      Return
    End Subroutine fdummy


  End Subroutine Fit1D

! ***********************************************
! *
  Subroutine FitMD(X, Y, Yerr, Func, Coef, Chisqr, logfile)
! *
! ***********************************************

    Real (kind=8), Intent (in) :: X(:,:), Y(:), Yerr(:)
    Real (kind=8), Intent (inout) :: Coef(:)
    Real (kind=8), Intent (out) :: Chisqr
    Character (len=*), Optional :: logfile
    
    Integer :: Idummy, I, Ierr, Ifoo
    Real (kind=8) :: foo
    Character (len=1) :: cfoo

    Interface 
       Function Func(X, C)
         Real (kind=8), Intent (in) :: X(:)
         Real (kind=8), Intent (in) :: C(:)
         Real (kind=8) :: Func
       End Function Func
    End Interface

    If (Present(logfile)) Then
       Open (unit=69, File=Trim(logfile))
    Else
       Open (unit=69, File='minuit.log')
    End If
    CALL MnInit(5,69,69)
    CALL Mncomd(Fm,"set pri -1",Ierr, fdummy)
    CALL Mncomd(Fm,"set now",Ierr, Fdummy)
    CALL Mncomd(Fm,"set str 2",Ierr, Fdummy)
    
    Do I = 1, Size(Coef)
       CALL MnParm(I, 'X', Coef(I), 1.0D0, 0.0D0, 0.0D0, Ierr)
    End Do

    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    CALL Mncomd(Fm,"seek",Ierr,Fdummy)
    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    
!    Output the Chisqr, and parameter value
    CALL MNstat(Chisqr, foo, foo, Ifoo, Ifoo, Ierr)
    Do I = 1, Size(Coef)
       CALL MnPout(I, cfoo, Coef(I), foo, foo, foo, Ierr)
    End Do

    Close(69)

    Return
  Contains

    Subroutine Fm(Npar,Grad,Fval,Xval,Iflag,Fdummy)
      
      Integer, Intent (in) :: Npar, Iflag
      Real (kind=8), Intent (out) :: Fval
      Real (kind=8), Intent (in) :: Xval(*), Grad(*), Fdummy
      
      Integer :: I
      Real (kind=8) :: C(Size(Coef))

      Do I = 1, Size(Coef)
         C(I) = Xval(I)
      End Do

      Fval = 0.0D0
      Do I = 1, Size(X,1)
         FVal = Fval + (Y(I) - Func(X(I,:),C))**2 / Yerr(I)**2 
      End Do

      Return
    End Subroutine Fm

    Subroutine fdummy()

      Return
    End Subroutine fdummy


  End Subroutine FitMD

! ***********************************************
! *
  Subroutine Fit1DCorr(X, Y, InvCorr, Func, Coef, Chisqr, logfile)
! *
! ***********************************************

    Real (kind=8), Intent (in) :: X(:), Y(:), InvCorr(:,:)
    Real (kind=8), Intent (inout) :: Coef(:)
    Real (kind=8), Intent (out) :: Chisqr
    Character (len=*), Optional :: logfile
    
    Integer :: Idummy, I, Ierr, Ifoo
    Real (kind=8) :: foo
    Character (len=1) :: cfoo

    Interface 
       Function Func(X, C)
         Real (kind=8), Intent (in) :: X
         Real (kind=8), Intent (in) :: C(:)
         Real (kind=8) :: Func
       End Function Func
    End Interface

    If (Present(logfile)) Then
       Open (unit=69, File=Trim(logfile))
    Else
       Open (unit=69, File='minuit.log')
    End If
    CALL MnInit(5,69,69)
    CALL Mncomd(Fm,"set pri -1",Ierr, fdummy)
    CALL Mncomd(Fm,"set now",Ierr, Fdummy)
    CALL Mncomd(Fm,"set str 2",Ierr, Fdummy)
    
    Do I = 1, Size(Coef)
       CALL MnParm(I, 'X', Coef(I), 1.0D0, 0.0D0, 0.0D0, Ierr)
    End Do

    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    CALL Mncomd(Fm,"seek",Ierr,Fdummy)
    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    
!    Output the Chisqr, and parameter value
    CALL MNstat(Chisqr, foo, foo, Ifoo, Ifoo, Ierr)
    Do I = 1, Size(Coef)
       CALL MnPout(I, cfoo, Coef(I), foo, foo, foo, Ierr)
    End Do

    Close(69)

    Return
  Contains

    Subroutine Fm(Npar,Grad,Fval,Xval,Iflag,Fdummy)
      
      Integer, Intent (in) :: Npar, Iflag
      Real (kind=8), Intent (out) :: Fval
      Real (kind=8), Intent (in) :: Xval(*), Grad(*), Fdummy
      
      Integer :: I, J
      Real (kind=8) :: C(Size(Coef))

      Do I = 1, Size(Coef)
         C(I) = Xval(I)
      End Do

      Fval = 0.0D0
      Do J = 1, Size(X)
         Do I = 1, Size(X)
            FVal = Fval + (Y(I) - Func(X(I),C)) * InvCorr(I,J) * &
                 & (Y(J) - Func(X(J),C))
         End Do
      End Do

      Return
    End Subroutine Fm

    Subroutine fdummy()

      Return
    End Subroutine fdummy


  End Subroutine Fit1DCorr

! ***********************************************
! *
  Subroutine FitMDCorr(X, Y, InvCorr, Func, Coef, Chisqr, logfile)
! *
! ***********************************************

    Real (kind=8), Intent (in) :: X(:,:), Y(:), InvCorr(:,:)
    Real (kind=8), Intent (inout) :: Coef(:)
    Real (kind=8), Intent (out) :: Chisqr
    Character (len=*), Optional :: logfile
    
    Integer :: Idummy, I, Ierr, Ifoo
    Real (kind=8) :: foo
    Character (len=1) :: cfoo

    Interface 
       Function Func(X, C)
         Real (kind=8), Intent (in) :: X(:)
         Real (kind=8), Intent (in) :: C(:)
         Real (kind=8) :: Func
       End Function Func
    End Interface

    If (Present(logfile)) Then
       Open (unit=69, File=Trim(logfile))
    Else
       Open (unit=69, File='minuit.log')
    End If
    CALL MnInit(5,69,69)
    CALL Mncomd(Fm,"set pri -1",Ierr, fdummy)
    CALL Mncomd(Fm,"set now",Ierr, Fdummy)
    CALL Mncomd(Fm,"set str 2",Ierr, Fdummy)
    
    Do I = 1, Size(Coef)
       CALL MnParm(I, 'X', Coef(I), 1.0D0, 0.0D0, 0.0D0, Ierr)
    End Do

    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    CALL Mncomd(Fm,"seek",Ierr,Fdummy)
    CALL Mncomd(Fm,"mini",Ierr,Fdummy)
    
!    Output the Chisqr, and parameter value
    CALL MNstat(Chisqr, foo, foo, Ifoo, Ifoo, Ierr)
    Do I = 1, Size(Coef)
       CALL MnPout(I, cfoo, Coef(I), foo, foo, foo, Ierr)
    End Do

    Close(69)

    Return
  Contains

    Subroutine Fm(Npar,Grad,Fval,Xval,Iflag,Fdummy)
      
      Integer, Intent (in) :: Npar, Iflag
      Real (kind=8), Intent (out) :: Fval
      Real (kind=8), Intent (in) :: Xval(*), Grad(*), Fdummy
      
      Integer :: I, J
      Real (kind=8) :: C(Size(Coef))

      Do I = 1, Size(Coef)
         C(I) = Xval(I)
      End Do

      Fval = 0.0D0
      Do J = 1, Size(X)
         Do I = 1, Size(X)
            FVal = Fval + (Y(I) - Func(X(I,:),C)) * InvCorr(I,J) * &
                 & (Y(J) - Func(X(J,:),C))
         End Do
      End Do

      Return
    End Subroutine Fm

    Subroutine fdummy()

      Return
    End Subroutine fdummy


  End Subroutine FitMDCorr



END MODULE MinuitAPI
