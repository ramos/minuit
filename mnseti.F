*
* $Id: mnseti.F,v 1.1.1.1 2000/06/08 11:19:20 andras Exp $
*
* $Log: mnseti.F,v $
* Revision 1.1.1.1  2000/06/08 11:19:20  andras
* import of MINUIT from CERNlib 2000
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNSETI(TIT)
#include "minuit/d506dp.inc"
CC       Called by user to set or change title of current task.
CC
#include "minuit/d506cm.inc"
      CHARACTER*(*) TIT
      CTITL = TIT
      RETURN
      END
