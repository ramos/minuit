*
* $Id: stand.F,v 1.1.1.1 2000/06/08 11:19:20 andras Exp $
*
* $Log: stand.F,v $
* Revision 1.1.1.1  2000/06/08 11:19:20  andras
* import of MINUIT from CERNlib 2000
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE STAND
#include "minuit/d506dp.inc"
CC        optional user-supplied subroutine is called whenever the
CC        command "standard" appears.
CC
      RETURN
      END

      LOGICAL FUNCTION INTRAC()
      
      INTRAC = .FALSE.
      RETURN
      END FUNCTION INTRAC

