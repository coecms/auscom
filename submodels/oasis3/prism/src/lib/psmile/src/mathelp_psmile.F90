
MODULE mathelp_psmile
!---------------------------------------------------------------------
!
! Reiner Vogelsang, SGI, 27 April 2003
! - Screening of 4 byte real interfaces in case a of dbl4 compilation.
!   File has to be preprocessed with -D__SXdbl4.
!
!
!---------------------------------------------------------------------
#include "psmile_os.h"

   USE errioipsl_psmile,ONLY : histerr
   USE stringop_psmile
!-
   PRIVATE
   PUBLIC :: mathop,moycum,trans_buff,buildop
!-
   INTERFACE buildop
#ifndef __NO_4BYTE_REALS
     MODULE PROCEDURE buildop_r4
#endif
     MODULE PROCEDURE buildop_r8
  end INTERFACE

   INTERFACE mathop
#ifndef __NO_4BYTE_REALS
     MODULE PROCEDURE mathop_r114
#endif
     MODULE PROCEDURE mathop_r118
   END INTERFACE

   INTERFACE moycum
#ifndef __NO_4BYTE_REALS
     MODULE PROCEDURE moycum_r4
#endif
     MODULE PROCEDURE moycum_r8
  end INTERFACE
   
!-
!- Variables used to detect and identify the operations
!-
   CHARACTER(LEN=80),SAVE :: &
  &  seps='( ) , + - / * ^', ops = '+ - * / ^', mima = 'min max'
   CHARACTER(LEN=250),SAVE :: &
  &  funcs = 'sin cos tan asin acos atan exp log sqrt chs abs '&
  & //'cels kelv deg rad gather scatter fill coll undef only ident'
   CHARACTER(LEN=120),SAVE :: &
  &  indexfu = 'gather, scatter, fill, coll, undef, only'
!---------------------------------------------------------------------
CONTAINS
!=
   SUBROUTINE buildop_r4 (str,ex_topps,topp,nbops_max, &
  &                    missing_val,opps,scal,nbops)
!---------------------------------------------------------------------
!- This subroutine decomposes the input string in the elementary
!- functions which need to be applied to the vector of data.
!- This vector is represented by X in the string.
!- This subroutine is the driver of the decomposition and gets
!- the time operation but then call decoop for the other operations
!-
!- INPUT
!-
!- str      : String containing the operations
!- ex_toops : The time operations that can be expected within the string
!-
!- OUTPUT
!-
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 ::str
   CHARACTER*(*) :: ex_topps
   CHARACTER*7 :: topp
   INTEGER (kind=ip_intwp_p) :: nbops_max,nbops
   CHARACTER*7 :: opps(nbops_max)
   REAL (kind=ip_single_p) :: scal(nbops_max),missing_val
!-
   CHARACTER*80 :: new_str
   INTEGER (kind=ip_intwp_p) :: leng,ind_opb,ind_clb
!-
   LOGICAL :: check = .FALSE.
!---------------------------------------------------------------------
   IF (check) WRITE(*,*) 'buildop : Some preliminary cleaning'
!-
   leng = LEN_TRIM(str)
   IF ( str(1:1) == '(' .AND. str(leng:leng) == ')' ) THEN
     str = str(2:leng-1)
     leng = leng-2
   ENDIF
!-
   IF (check) &
  &  WRITE(*,*) 'buildop : Starting to test the various options'
!-
   IF (leng <= 5 .AND. INDEX(ex_topps,str(1:leng)) > 0) THEN
     IF (check) WRITE(*,*) 'buildop : Time operation only'
     nbops = 0
     topp = str(1:leng)
   ELSE
     IF (check) WRITE(*,*) 'buildop : Time operation and something else'
!---
     ind_opb = INDEX(str(1:leng),'(')
     IF (ind_opb > 0) THEN
       IF ( INDEX(ex_topps,str(1:ind_opb-1)) > 0) THEN
         IF (check) WRITE(*,'(2a)') &
                  &  ' buildop : Extract time operation from : ',str
         topp = str(1:ind_opb-1)
         ind_clb = INDEX(str(1:leng),')',BACK=.TRUE.)
         new_str = str(ind_opb+1:ind_clb-1)
         IF (check) WRITE(*,'(2a,2I3)') &
                  &  ' buildop : Call decoop ',new_str,ind_opb,ind_clb
         CALL decoop_r4 (new_str,nbops_max,missing_val,opps,scal,nbops)
       ELSE
         CALL histerr(3,'buildop','time opperation does not exist',str(1:ind_opb-1),' ')
       ENDIF
     ELSE
       CALL histerr(3,'buildop','some long opperation exists but wihout parenthesis',str(1:leng),' ')
     ENDIF
   ENDIF
!-
   IF (check) THEN
     DO leng=1,nbops
       WRITE(*,*) 'buildop : i -- opps, scal : ',leng,opps(leng),scal(leng)
     ENDDO
   ENDIF
!------------------------
 END SUBROUTINE buildop_r4
!=
   SUBROUTINE buildop_r8 (str,ex_topps,topp,nbops_max, &
  &                    missing_val,opps,scal,nbops)
!---------------------------------------------------------------------
!- This subroutine decomposes the input string in the elementary
!- functions which need to be applied to the vector of data.
!- This vector is represented by X in the string.
!- This subroutine is the driver of the decomposition and gets
!- the time operation but then call decoop for the other operations
!-
!- INPUT
!-
!- str      : String containing the operations
!- ex_toops : The time operations that can be expected within the string
!-
!- OUTPUT
!-
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 ::str
   CHARACTER*(*) :: ex_topps
   CHARACTER*7 :: topp
   INTEGER (kind=ip_intwp_p) :: nbops_max,nbops
   CHARACTER*7 :: opps(nbops_max)
   REAL (kind=ip_double_p) :: scal(nbops_max),missing_val
!-
   CHARACTER*80 :: new_str
   INTEGER (kind=ip_intwp_p) :: leng,ind_opb,ind_clb
!-
   LOGICAL :: check = .FALSE.
!---------------------------------------------------------------------
   IF (check) WRITE(*,*) 'buildop : Some preliminary cleaning'
!-
   leng = LEN_TRIM(str)
   IF ( str(1:1) == '(' .AND. str(leng:leng) == ')' ) THEN
     str = str(2:leng-1)
     leng = leng-2
   ENDIF
!-
   IF (check) &
  &  WRITE(*,*) 'buildop : Starting to test the various options'
!-
   IF (leng <= 5 .AND. INDEX(ex_topps,str(1:leng)) > 0) THEN
     IF (check) WRITE(*,*) 'buildop : Time operation only'
     nbops = 0
     topp = str(1:leng)
   ELSE
     IF (check) WRITE(*,*) 'buildop : Time operation and something else'
!---
     ind_opb = INDEX(str(1:leng),'(')
     IF (ind_opb > 0) THEN
       IF ( INDEX(ex_topps,str(1:ind_opb-1)) > 0) THEN
         IF (check) WRITE(*,'(2a)') &
                  &  ' buildop : Extract time operation from : ',str
         topp = str(1:ind_opb-1)
         ind_clb = INDEX(str(1:leng),')',BACK=.TRUE.)
         new_str = str(ind_opb+1:ind_clb-1)
         IF (check) WRITE(*,'(2a,2I3)') &
                  &  ' buildop : Call decoop ',new_str,ind_opb,ind_clb
         CALL decoop_r8 (new_str,nbops_max,missing_val,opps,scal,nbops)
       ELSE
         CALL histerr(3,'buildop','time opperation does not exist',str(1:ind_opb-1),' ')
       ENDIF
     ELSE
       CALL histerr(3,'buildop','some long opperation exists but wihout parenthesis',str(1:leng),' ')
     ENDIF
   ENDIF
!-
   IF (check) THEN
     DO leng=1,nbops
       WRITE(*,*) 'buildop : i -- opps, scal : ',leng,opps(leng),scal(leng)
     ENDDO
   ENDIF
!------------------------
 END SUBROUTINE buildop_r8
!=
   SUBROUTINE decoop_r4 (pstr,nbops_max,missing_val,opps,scal,nbops)
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 :: pstr
   INTEGER (kind=ip_intwp_p) :: nbops_max,nbops
   CHARACTER*7 :: opps(nbops_max)
   REAL (kind=ip_single_p) :: scal(nbops_max),missing_val
!-
   CHARACTER*1 :: f_char(2),s_char(2)
   INTEGER (kind=ip_intwp_p) :: nbsep,f_pos(2),s_pos(2)
   CHARACTER*20 :: opp_str,scal_str
   CHARACTER*80 :: str
   INTEGER (kind=ip_intwp_p) :: xpos,leng,ppos,epos,int_tmp
   CHARACTER*3 :: tl,dl
   CHARACTER*10 :: fmt
!-
   LOGICAL :: check = .FALSE.,prio
!---------------------------------------------------------------------
   IF (check) WRITE(*,'(2a)') ' decoop : Incoming string : ',pstr
!-
   nbops = 0
   str = pstr
!-
   CALL findsep (str,nbsep,f_char,f_pos,s_char,s_pos)
   IF (check) WRITE(*,*) 'decoop : Out of findsep',nbsep
   DO WHILE (nbsep > 0)
     xpos = INDEX(str,'X')
     leng = LEN_TRIM(str)
     nbops = nbops+1
!---
     IF (check) THEN
       WRITE(*,*) 'decoop : str -->',str(1:leng)
       WRITE(*,*) s_char(1),'-',f_char(1),'|',f_char(2),'-',s_char(2)
       WRITE(*,*) s_pos(1),'-',f_pos(1),'|',f_pos(2),'-',s_pos(2)
     ENDIF
!---
     IF (nbops > nbops_max-1) THEN
       CALL histerr(3,'decoop','Expression too complex',str,' ')
     ENDIF
!---
     IF (check) WRITE(*,*) 'decoop : --',nbops,' ',str(1:leng)
!---
!--- Start the analysis of the syntax. 3 types of constructs
!--- are recognized.  They are scanned sequentialy
!---
     IF (nbsep == 1) THEN
       IF (check) WRITE(*,*) 'decoop : Only one operation'
       IF (INDEX(ops,f_char(1)) > 0) THEN
!-------
!------- Type : scal+X
!-------
         IF (f_char(1) == '-' .OR. f_char(1) == '/') THEN
           opp_str = f_char(1)//'I'
         ELSE
           opp_str = f_char(1)
         ENDIF
         scal_str = str(s_pos(1)+1:f_pos(1)-1)
         str = 'X'
       ELSE IF (INDEX(ops,f_char(2)) > 0) THEN
!-------
!------- Type : X+scal
!-------
         opp_str = f_char(2)
         scal_str = str(f_pos(2)+1:s_pos(2)-1)
         str = 'X'
       ELSE
         CALL histerr(3,'decoop','Unknown operations of type X+scal',f_char(1),pstr)
       ENDIF
     ELSE
       IF (check) WRITE(*,*) 'decoop : More complex operation'
       IF ( f_char(1) == '(' .AND. f_char(2) == ')' ) THEN
!-------
!------- Type : sin(X)
!-------
         opp_str = str(s_pos(1)+1:f_pos(1)-1)
         scal_str = '?'
         str = str(1:s_pos(1))//'X'//str(f_pos(2)+1:leng)
       ELSE IF (    (f_char(1) == '(' .AND. f_char(2) == ',')&
      &         .OR.(f_char(1) == ',' .AND. f_char(2) == ')')) THEN
!-------
!------- Type : max(X,scal) or max(scal,X)
!-------
         IF (f_char(1) == '(' .AND. s_char(2) == ')') THEN
!---------
!--------- Type : max(X,scal)
!---------
           opp_str = str(f_pos(1)-3:f_pos(1)-1)
           scal_str = str(f_pos(2)+1:s_pos(2)-1)
           str = str(1:f_pos(1)-4)//'X'//str(s_pos(2)+1:leng)
         ELSE IF (f_char(1) == ',' .AND. s_char(1) == '(') THEN
!---------
!--------- Type : max(scal,X)
!---------
           opp_str = str(s_pos(1)-3:s_pos(1)-1)
           scal_str = str(s_pos(1)+1:f_pos(1)-1)
           str = str(1:s_pos(1)-4)//'X'//str(f_pos(2)+1:leng)
         ELSE
           CALL histerr(3,'decoop','Syntax error 1',str,' ')
         ENDIF
       ELSE
         prio = (f_char(2) == '*').OR.(f_char(2) == '^')
         IF (     (INDEX(ops,f_char(1)) > 0) &
        &    .AND.(xpos-f_pos(1) == 1).AND.(.NOT.prio) ) THEN
!---------
!--------- Type : ... scal+X ...
!---------
           IF (f_char(1) == '-' .OR. f_char(1) == '/') THEN
             opp_str = f_char(1)//'I'
           ELSE
             opp_str = f_char(1)
           ENDIF
           scal_str = str(s_pos(1)+1:f_pos(1)-1)
           str = str(1:s_pos(1))//'X'//str(f_pos(1)+2:leng)
         ELSE IF (     (INDEX(ops,f_char(2)) > 0) &
        &         .AND.(f_pos(2)-xpos == 1) ) THEN
!---------
!--------- Type : ... X+scal ...
!---------
           opp_str = f_char(2)
           scal_str = str(f_pos(2)+1:s_pos(2)-1)
           str = str(1:f_pos(2)-2)//'X'//str(s_pos(2):leng)
         ELSE
           CALL histerr(3,'decoop','Syntax error 2',str,' ')
         ENDIF
       ENDIF
     ENDIF
!---
     IF (check) WRITE(*,*) 'decoop : Finished syntax,str = ',str(1:LEN_TRIM(str))
!-----
!----- Now that the different components of the operation are identified
!----- we transform them into what is going to be used in the program
!-----
     IF (INDEX(scal_str,'?') > 0) THEN
       IF (INDEX(funcs,opp_str(1:LEN_TRIM(opp_str))) > 0) THEN
         opps(nbops) = opp_str(1:LEN_TRIM(opp_str))
         scal(nbops) =  missing_val
       ELSE
         CALL histerr(3,'decoop','Unknown function',opp_str(1:LEN_TRIM(opp_str)),' ')
       ENDIF
     ELSE
       leng = LEN_TRIM(opp_str)
       IF (INDEX(mima,opp_str(1:leng)) > 0) THEN
         opps(nbops) = 'fu'//opp_str(1:leng)
       ELSE
         IF (INDEX(opp_str(1:leng),'+') > 0) THEN
           opps(nbops) = 'add'
         ELSE IF (INDEX(opp_str(1:leng),'-I') > 0) THEN
           opps(nbops) = 'subi'
         ELSE IF (INDEX(opp_str(1:leng),'-') > 0) THEN
           opps(nbops) = 'sub'
         ELSE IF (INDEX(opp_str(1:leng),'*') > 0) THEN
           opps(nbops) = 'mult'
         ELSE IF (INDEX(opp_str(1:leng),'/') > 0) THEN
           opps(nbops) = 'div'
         ELSE IF (INDEX(opp_str(1:leng),'/I') > 0) THEN
           opps(nbops) = 'divi'
         ELSE IF (INDEX(opp_str(1:leng),'^') > 0) THEN
           opps(nbops) = 'power'
         ELSE
           CALL histerr(3,'decoop','Unknown operation',opp_str(1:leng),' ')
         ENDIF
       ENDIF
!-----
       leng = LEN_TRIM(scal_str)
       ppos = INDEX(scal_str,'.')
       epos = INDEX(scal_str,'e')
       IF (epos == 0) epos = INDEX(scal_str,'E')
!-----
!----- Try to catch a few errors
!-----
       IF (INDEX(ops,scal_str) > 0) THEN
         CALL histerr(3,'decoop','Strange scalar you have here ',scal_str,pstr)
       ENDIF
       IF (epos > 0) THEN
         WRITE(tl,'(I3.3)') leng
         WRITE(dl,'(I3.3)') epos-ppos-1
         fmt='(e'//tl//'.'//dl//')'
         READ(scal_str,fmt) scal(nbops)
       ELSE IF (ppos > 0) THEN
         WRITE(tl,'(I3.3)') leng
         WRITE(dl,'(I3.3)') leng-ppos
         fmt='(f'//tl//'.'//dl//')'
         READ(scal_str,fmt) scal(nbops)
       ELSE
         WRITE(tl,'(I3.3)') leng
         fmt = '(I'//tl//')'
         READ(scal_str,fmt) int_tmp
         scal(nbops) = REAL(int_tmp, kind=ip_single_p)
       ENDIF
     ENDIF
     IF (check) WRITE(*,*) 'decoop : Finished interpretation'
     CALL findsep(str,nbsep,f_char,f_pos,s_char,s_pos)
   ENDDO
!-----------------------
 END SUBROUTINE decoop_r4
!=
   SUBROUTINE decoop_r8 (pstr,nbops_max,missing_val,opps,scal,nbops)
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 :: pstr
   INTEGER (kind=ip_intwp_p) :: nbops_max,nbops
   CHARACTER*7 :: opps(nbops_max)
   REAL (kind=ip_double_p) :: scal(nbops_max),missing_val
!-
   CHARACTER*1 :: f_char(2),s_char(2)
   INTEGER (kind=ip_intwp_p) :: nbsep,f_pos(2),s_pos(2)
   CHARACTER*20 :: opp_str,scal_str
   CHARACTER*80 :: str
   INTEGER (kind=ip_intwp_p) :: xpos,leng,ppos,epos,int_tmp
   CHARACTER*3 :: tl,dl
   CHARACTER*10 :: fmt
!-
   LOGICAL :: check = .FALSE.,prio
!---------------------------------------------------------------------
   IF (check) WRITE(*,'(2a)') ' decoop : Incoming string : ',pstr
!-
   nbops = 0
   str = pstr
!-
   CALL findsep (str,nbsep,f_char,f_pos,s_char,s_pos)
   IF (check) WRITE(*,*) 'decoop : Out of findsep',nbsep
   DO WHILE (nbsep > 0)
     xpos = INDEX(str,'X')
     leng = LEN_TRIM(str)
     nbops = nbops+1
!---
     IF (check) THEN
       WRITE(*,*) 'decoop : str -->',str(1:leng)
       WRITE(*,*) s_char(1),'-',f_char(1),'|',f_char(2),'-',s_char(2)
       WRITE(*,*) s_pos(1),'-',f_pos(1),'|',f_pos(2),'-',s_pos(2)
     ENDIF
!---
     IF (nbops > nbops_max-1) THEN
       CALL histerr(3,'decoop','Expression too complex',str,' ')
     ENDIF
!---
     IF (check) WRITE(*,*) 'decoop : --',nbops,' ',str(1:leng)
!---
!--- Start the analysis of the syntax. 3 types of constructs
!--- are recognized.  They are scanned sequentialy
!---
     IF (nbsep == 1) THEN
       IF (check) WRITE(*,*) 'decoop : Only one operation'
       IF (INDEX(ops,f_char(1)) > 0) THEN
!-------
!------- Type : scal+X
!-------
         IF (f_char(1) == '-' .OR. f_char(1) == '/') THEN
           opp_str = f_char(1)//'I'
         ELSE
           opp_str = f_char(1)
         ENDIF
         scal_str = str(s_pos(1)+1:f_pos(1)-1)
         str = 'X'
       ELSE IF (INDEX(ops,f_char(2)) > 0) THEN
!-------
!------- Type : X+scal
!-------
         opp_str = f_char(2)
         scal_str = str(f_pos(2)+1:s_pos(2)-1)
         str = 'X'
       ELSE
         CALL histerr(3,'decoop','Unknown operations of type X+scal',f_char(1),pstr)
       ENDIF
     ELSE
       IF (check) WRITE(*,*) 'decoop : More complex operation'
       IF ( f_char(1) == '(' .AND. f_char(2) == ')' ) THEN
!-------
!------- Type : sin(X)
!-------
         opp_str = str(s_pos(1)+1:f_pos(1)-1)
         scal_str = '?'
         str = str(1:s_pos(1))//'X'//str(f_pos(2)+1:leng)
       ELSE IF (    (f_char(1) == '(' .AND. f_char(2) == ',')&
      &         .OR.(f_char(1) == ',' .AND. f_char(2) == ')')) THEN
!-------
!------- Type : max(X,scal) or max(scal,X)
!-------
         IF (f_char(1) == '(' .AND. s_char(2) == ')') THEN
!---------
!--------- Type : max(X,scal)
!---------
           opp_str = str(f_pos(1)-3:f_pos(1)-1)
           scal_str = str(f_pos(2)+1:s_pos(2)-1)
           str = str(1:f_pos(1)-4)//'X'//str(s_pos(2)+1:leng)
         ELSE IF (f_char(1) == ',' .AND. s_char(1) == '(') THEN
!---------
!--------- Type : max(scal,X)
!---------
           opp_str = str(s_pos(1)-3:s_pos(1)-1)
           scal_str = str(s_pos(1)+1:f_pos(1)-1)
           str = str(1:s_pos(1)-4)//'X'//str(f_pos(2)+1:leng)
         ELSE
           CALL histerr(3,'decoop','Syntax error 1',str,' ')
         ENDIF
       ELSE
         prio = (f_char(2) == '*').OR.(f_char(2) == '^')
         IF (     (INDEX(ops,f_char(1)) > 0) &
        &    .AND.(xpos-f_pos(1) == 1).AND.(.NOT.prio) ) THEN
!---------
!--------- Type : ... scal+X ...
!---------
           IF (f_char(1) == '-' .OR. f_char(1) == '/') THEN
             opp_str = f_char(1)//'I'
           ELSE
             opp_str = f_char(1)
           ENDIF
           scal_str = str(s_pos(1)+1:f_pos(1)-1)
           str = str(1:s_pos(1))//'X'//str(f_pos(1)+2:leng)
         ELSE IF (     (INDEX(ops,f_char(2)) > 0) &
        &         .AND.(f_pos(2)-xpos == 1) ) THEN
!---------
!--------- Type : ... X+scal ...
!---------
           opp_str = f_char(2)
           scal_str = str(f_pos(2)+1:s_pos(2)-1)
           str = str(1:f_pos(2)-2)//'X'//str(s_pos(2):leng)
         ELSE
           CALL histerr(3,'decoop','Syntax error 2',str,' ')
         ENDIF
       ENDIF
     ENDIF
!---
     IF (check) WRITE(*,*) 'decoop : Finished syntax,str = ',str(1:LEN_TRIM(str))
!-----
!----- Now that the different components of the operation are identified
!----- we transform them into what is going to be used in the program
!-----
     IF (INDEX(scal_str,'?') > 0) THEN
       IF (INDEX(funcs,opp_str(1:LEN_TRIM(opp_str))) > 0) THEN
         opps(nbops) = opp_str(1:LEN_TRIM(opp_str))
         scal(nbops) =  missing_val
       ELSE
         CALL histerr(3,'decoop','Unknown function',opp_str(1:LEN_TRIM(opp_str)),' ')
       ENDIF
     ELSE
       leng = LEN_TRIM(opp_str)
       IF (INDEX(mima,opp_str(1:leng)) > 0) THEN
         opps(nbops) = 'fu'//opp_str(1:leng)
       ELSE
         IF (INDEX(opp_str(1:leng),'+') > 0) THEN
           opps(nbops) = 'add'
         ELSE IF (INDEX(opp_str(1:leng),'-I') > 0) THEN
           opps(nbops) = 'subi'
         ELSE IF (INDEX(opp_str(1:leng),'-') > 0) THEN
           opps(nbops) = 'sub'
         ELSE IF (INDEX(opp_str(1:leng),'*') > 0) THEN
           opps(nbops) = 'mult'
         ELSE IF (INDEX(opp_str(1:leng),'/') > 0) THEN
           opps(nbops) = 'div'
         ELSE IF (INDEX(opp_str(1:leng),'/I') > 0) THEN
           opps(nbops) = 'divi'
         ELSE IF (INDEX(opp_str(1:leng),'^') > 0) THEN
           opps(nbops) = 'power'
         ELSE
           CALL histerr(3,'decoop','Unknown operation',opp_str(1:leng),' ')
         ENDIF
       ENDIF
!-----
       leng = LEN_TRIM(scal_str)
       ppos = INDEX(scal_str,'.')
       epos = INDEX(scal_str,'e')
       IF (epos == 0) epos = INDEX(scal_str,'E')
!-----
!----- Try to catch a few errors
!-----
       IF (INDEX(ops,scal_str) > 0) THEN
         CALL histerr(3,'decoop','Strange scalar you have here ',scal_str,pstr)
       ENDIF
       IF (epos > 0) THEN
         WRITE(tl,'(I3.3)') leng
         WRITE(dl,'(I3.3)') epos-ppos-1
         fmt='(e'//tl//'.'//dl//')'
         READ(scal_str,fmt) scal(nbops)
       ELSE IF (ppos > 0) THEN
         WRITE(tl,'(I3.3)') leng
         WRITE(dl,'(I3.3)') leng-ppos
         fmt='(f'//tl//'.'//dl//')'
         READ(scal_str,fmt) scal(nbops)
       ELSE
         WRITE(tl,'(I3.3)') leng
         fmt = '(I'//tl//')'
         READ(scal_str,fmt) int_tmp
         scal(nbops) = REAL(int_tmp,kind=ip_single_p)
       ENDIF
     ENDIF
     IF (check) WRITE(*,*) 'decoop : Finished interpretation'
     CALL findsep(str,nbsep,f_char,f_pos,s_char,s_pos)
   ENDDO
!-----------------------
 END SUBROUTINE decoop_r8
!=
   SUBROUTINE findsep (str,nbsep,f_char,f_pos,s_char,s_pos)
!---------------------------------------------------------------------
!- Subroutine finds all separators in a given string
!- It returns the following information about str :
!-   f_char : The first separation character
!-            (1 for before and 2 for after)
!-   f_pos  : The position of the first separator
!-   s_char : The second separation character
!-            (1 for before and 2 for after)
!-   s_pos  : The position of the second separator
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 :: str
   INTEGER (kind=ip_intwp_p) :: nbsep
   CHARACTER*1 :: f_char(2),s_char(2)
   INTEGER (kind=ip_intwp_p) :: f_pos(2),s_pos(2)
!-
   CHARACTER*70 :: str_tmp
   LOGICAL :: f_found,s_found
   INTEGER (kind=ip_intwp_p)  ::ind,xpos,leng,i
!-
   LOGICAL :: check = .FALSE.
!---------------------------------------------------------------------
   IF (check) &
  &  WRITE(*,*) 'findsep : call cleanstr: ',str(1:LEN_TRIM(str))
!-
   CALL cleanstr(str)
!-
   IF (check) &
  &  WRITE(*,*) 'findsep : out of cleanstr: ',str(1:LEN_TRIM(str))
!-
   xpos = INDEX(str,'X')
   leng = LEN_TRIM(str)
!-
   f_pos(1:2) = (/ 0,leng+1 /)
   f_char(1:2) = (/ '?','?' /)
   s_pos(1:2) = (/ 0,leng+1 /)
   s_char(1:2) = (/ '?','?' /)
!-
   nbsep = 0
!-
   f_found = .FALSE.
   s_found = .FALSE.
   IF (xpos > 1) THEN
     DO i=xpos-1,1,-1
       ind = INDEX(seps,str(i:i))
       IF (ind > 0) THEN
         IF (.NOT.f_found) THEN
           f_char(1) = str(i:i)
           f_pos(1) = i
           nbsep = nbsep+1
           f_found = .TRUE.
         ELSE IF (.NOT.s_found) THEN
           s_char(1) = str(i:i)
           s_pos(1) = i
           nbsep = nbsep+1
           s_found = .TRUE.
         ENDIF
       ENDIF
     ENDDO
   ENDIF
!-
   f_found = .FALSE.
   s_found = .FALSE.
   IF (xpos < leng) THEN
     DO i=xpos+1,leng
       ind = INDEX(seps,str(i:i))
       IF (ind > 0) THEN
         IF (.NOT.f_found) THEN
           f_char(2) = str(i:i)
           f_pos(2) = i
           nbsep = nbsep+1
           f_found = .TRUE.
         ELSE IF (.NOT.s_found) THEN
           s_char(2) = str(i:i)
           s_pos(2) = i
           nbsep = nbsep+1
           s_found = .TRUE.
         ENDIF
       ENDIF
     ENDDO
   ENDIF
!-
   IF (nbsep > 4) THEN
     WRITE(str_tmp,'("number :",I3)') nbsep
     CALL histerr(3,'findsep','How can I find that many separators',str_tmp,str)
   ENDIF
!-
   IF (check) WRITE(*,*) 'Finished findsep : ',nbsep,leng
!------------------------
   END SUBROUTINE findsep
!=
   SUBROUTINE cleanstr(str)
!---------------------------------------------------------------------
!- We clean up the string by taking out the extra () and puting
!- everything in lower case except for the X describing the variable
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*80 :: str
!-
   INTEGER (kind=ip_intwp_p) :: ind,leng,ic,it
   LOGICAL :: check = .FALSE.
!---------------------------------------------------------------------
   leng = LEN_TRIM(str)
   CALL strlowercase(str)
!-
   ind = INDEX(str,'x')
   IF (check) WRITE (*,*) 'cleanstr 1.0 : ind = ',ind,' str = ',str(1:leng),'---'
!-
!- If the character before the x is not a letter then we can assume that
!- it is the variable and promote it to a capital letter
!-
   DO WHILE (ind > 0)
     ic = 0
     IF (ind > 1) ic = IACHAR(str(ind-1:ind-1))
     IF (ic < 97 .OR. ic > 122) THEN
       str(ind:ind) = 'X'
     ENDIF
     it = INDEX(str(ind+1:leng),'x')
     IF (it > 0) THEN
       ind = ind+it
     ELSE
       ind = it
     ENDIF
   ENDDO
!-
   IF (check) WRITE (*,*) 'cleanstr 2.0 : str = ',str(1:leng),'---'
!-
   IF ( str(1:1) == '(' .AND. str(leng:leng) == ')' ) THEN
     str = str(2:leng-1)
   ENDIF
!-
   IF (check) WRITE (*,*) 'cleanstr 3.0 : str = ',str(1:leng),'---'
!-
   leng = LEN_TRIM(str)
   ind = INDEX(str,'((X))')
   IF (ind > 0) THEN
     str=str(1:ind-1)//'(X)'//str(ind+5:leng)//'  '
   ENDIF
!-
   IF (check) WRITE (*,*) 'cleanstr 4.0 : str = ',str(1:leng),'---'
!-
   leng = LEN_TRIM(str)
   ind = INDEX(str,'(X)')
   IF (ind > 0 .AND. ind+3 < leng) THEN
     IF ( INDEX(seps,str(ind-1:ind-1)) > 0 .AND. INDEX(seps,str(ind+3:ind+3)) > 0) THEN
       str=str(1:ind-1)//'X'//str(ind+3:leng)//'  '
     ENDIF
   ENDIF
!-
   IF (check) WRITE (*,*) 'cleanstr 5.0 : str = ',str(1:leng),'---'
!-
   leng = LEN_TRIM(str)
   ind = INDEX(str(1:leng),' ')
   DO WHILE (ind > 0)
     str=str(1:ind-1)//str(ind+1:leng)//' '
     leng = LEN_TRIM(str)
     ind = INDEX(str(1:leng),' ')
   ENDDO
!-
   IF (check) WRITE (*,*) 'cleanstr 6.0 : str = ',str(1:leng),'---'
!-------------------------
   END SUBROUTINE cleanstr
!=
   SUBROUTINE mathop_r114(fun,nb,work_in,miss_val,nb_index,nindex,scal,nb_max,work_out)
!---------------------------------------------------------------------
!- This soubroutines gives an interface to the various operation
!- which are allowed. The interface is general enough to allow its use
!- for other cases.
!-
!- INPUT
!-
!- fun      : function to be applied to the vector of data
!- nb       : Length of input vector
!- work_in  : Input vector of data (REAL (kind=ip_single_p))
!- miss_val : The value of the missing data flag (it has to be a
!-            maximum value, in f90 : huge( a real ))
!- nb_index : Length of index vector
!- nindex   : Vector of indices
!- scal     : A scalar value for vector/scalar operations
!- nb_max   : maximum length of output vector
!-
!- OUTPUT
!-
!- nb_max   : Actual length of output variable
!- work_out : Output vector after the operation was applied
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*7 :: fun
   INTEGER (kind=ip_intwp_p):: nb,nb_max,nb_index
   INTEGER (kind=ip_intwp_p):: nindex(nb_index)
   REAL (kind=ip_single_p) :: miss_val
   REAL (kind=ip_single_p) ::work_in(nb),scal
   REAL (kind=ip_single_p) ::work_out(nb_max)
!-
   INTEGER (kind=ip_intwp_p):: ierr
!-
   INTRINSIC SIN,COS,TAN,ASIN,ACOS,ATAN,EXP,ALOG,SQRT,ABS
!---------------------------------------------------------------------
   ierr = 0
!-
   IF (scal >= miss_val-1.) THEN
     IF (INDEX(indexfu,fun(1:LEN_TRIM(fun))) == 0) THEN
       SELECT CASE (fun)
       CASE('sin')
         ierr = ma_sin_r114(nb,work_in,nb_max,work_out)
       CASE('cos')
         ierr = ma_cos_r114(nb,work_in,nb_max,work_out)
       CASE('tan')
         ierr = ma_tan_r114(nb,work_in,nb_max,work_out)
       CASE('asin')
         ierr = ma_asin_r114(nb,work_in,nb_max,work_out)
       CASE('acos')
         ierr = ma_acos_r114(nb,work_in,nb_max,work_out)
       CASE('atan')
         ierr = ma_atan_r114(nb,work_in,nb_max,work_out)
       CASE('exp')
         ierr = ma_exp_r114(nb,work_in,nb_max,work_out)
       CASE('log')
         ierr = ma_alog_r114(nb,work_in,nb_max,work_out)
       CASE('sqrt')
         ierr = ma_sqrt_r114(nb,work_in,nb_max,work_out)
       CASE('chs')
         ierr = ma_chs_r114(nb,work_in,nb_max,work_out)
       CASE('abs')
         ierr = ma_abs_r114(nb,work_in,nb_max,work_out)
       CASE('cels')
         ierr = ma_cels_r114(nb,work_in,nb_max,work_out)
       CASE('kelv')
         ierr = ma_kelv_r114(nb,work_in,nb_max,work_out)
       CASE('deg')
         ierr = ma_deg_r114(nb,work_in,nb_max,work_out)
       CASE('rad')
         ierr = ma_rad_r114(nb,work_in,nb_max,work_out)
       CASE('ident')
         ierr = ma_ident_r114(nb,work_in,nb_max,work_out)
       CASE DEFAULT
         CALL histerr(3,"mathop",'scalar variable undefined and no indexing','but still unknown function',fun)
       END SELECT
       IF (ierr > 0) THEN
         CALL histerr(3,"mathop",'Error while executing a simple function',fun,' ')
       ENDIF
     ELSE
       SELECT CASE (fun)
       CASE('gather')
         ierr = ma_fugath_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('scatter')
         IF (nb_index > nb) THEN
           DO ierr=1,nb_max
             work_out(ierr)=miss_val
           ENDDO 
           ierr=1
         ELSE
           ierr = ma_fuscat_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
         ENDIF
       CASE('coll')
         ierr = ma_fucoll_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('fill')
         ierr = ma_fufill_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('undef')
         ierr = ma_fuundef_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('only')
         ierr = ma_fuonly_r114(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE DEFAULT
         CALL histerr(3,"mathop",'scalar variable undefined and indexing',&
        &              'was requested but with unknown function',fun)
       END SELECT
       IF (ierr > 0) THEN
         CALL histerr(3,"mathop_r114",'Error while executing an indexing function',fun,' ')
       ENDIF
     ENDIF
   ELSE
     SELECT CASE (fun)
     CASE('fumin')
       ierr = ma_fumin_r114(nb,work_in,scal,nb_max,work_out)
     CASE('fumax')
       ierr = ma_fumax_r114(nb,work_in,scal,nb_max,work_out)
     CASE('add')
       ierr = ma_add_r114(nb,work_in,scal,nb_max,work_out)
     CASE('subi')
       ierr = ma_subi_r114(nb,work_in,scal,nb_max,work_out)
     CASE('sub')
       ierr = ma_sub_r114(nb,work_in,scal,nb_max,work_out)
     CASE('mult')
       ierr = ma_mult_r114(nb,work_in,scal,nb_max,work_out)
     CASE('div')
       ierr = ma_div_r114(nb,work_in,scal,nb_max,work_out)
     CASE('divi')
       ierr = ma_divi_r114(nb,work_in,scal,nb_max,work_out)
     CASE('power')
       ierr = ma_power_r114(nb,work_in,scal,nb_max,work_out)
     CASE DEFAULT
       CALL histerr(3,"mathop",'Unknown operation with a scalar',fun,' ')
     END SELECT
     IF (ierr > 0) THEN
       CALL histerr(3,"mathop",'Error while executing a scalar function',fun,' ')
     ENDIF
   ENDIF
!---------------------------
 END SUBROUTINE mathop_r114
  !
  !- INCLUDE 'mafunc_r11.inc'
  !------------------------------------------------------
  !------------------------------------------------------
  !    FUNCTIONS (only one argument)
  !------------------------------------------------------
  !------------------------------------------------------
   FUNCTION ma_sin_r114(nb,x,nbo,y)
   USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p) :: ma_sin_r114 
    INTEGER (kind=ip_intwp_p) :: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = SIN(x(i))
    ENDDO

    nbo = nb
    ma_sin_r114 = 0

  END FUNCTION ma_sin_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_cos_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p) :: ma_cos_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = COS(x(i))
    ENDDO

    nbo = nb
    ma_cos_r114 = 0

  END FUNCTION ma_cos_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_tan_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p) :: ma_tan_r114 

    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = TAN(x(i))
    ENDDO

    nbo = nb
    ma_tan_r114 = 0

  END FUNCTION ma_tan_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_asin_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p) ma_asin_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ASIN(x(i))
    ENDDO

    nbo = nb
    ma_asin_r114 = 0

  END FUNCTION ma_asin_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_acos_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_acos_r114 
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ACOS(x(i))
    ENDDO

    nbo = nb
    ma_acos_r114 = 0

  END FUNCTION ma_acos_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_atan_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER  (kind=ip_intwp_p) :: ma_atan_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ATAN(x(i))
    ENDDO

    nbo = nb
    ma_atan_r114 = 0

  END FUNCTION ma_atan_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_exp_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_exp_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = EXP(x(i))
    ENDDO

    nbo = nb
    ma_exp_r114 = 0

  END FUNCTION ma_exp_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_alog_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_alog_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = log(x(i))
    ENDDO

    nbo = nb
    ma_alog_r114 = 0

  END FUNCTION ma_alog_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_sqrt_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_sqrt_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = SQRT(x(i))
    ENDDO

    nbo = nb
    ma_sqrt_r114 = 0

  END FUNCTION ma_sqrt_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_abs_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_abs_r114 
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ABS(x(i))
    ENDDO

    nbo = nb
    ma_abs_r114 = 0

  END FUNCTION ma_abs_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_chs_r114(nb,x,nbo,y)
    USE mod_kinds_model
    IMPLICIT NONE
    INTEGER (kind=ip_intwp_p):: ma_chs_r114
    INTEGER (kind=ip_intwp_p):: nb,nbo,i
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)*(-1.)
    ENDDO

    nbo = nb
    ma_chs_r114 = 0

  END FUNCTION ma_chs_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_cels_r114(nb,x,nbo,y)
    USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_cels_r114
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)-273.15
    ENDDO

    nbo = nb
    ma_cels_r114 = 0


  END FUNCTION ma_cels_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_kelv_r114(nb,x,nbo,y)
    USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_kelv_r114
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)+273.15
    ENDDO

    nbo = nb
    ma_kelv_r114 = 0

  END FUNCTION ma_kelv_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_deg_r114(nb,x,nbo,y)
    USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_deg_r114
    REAL (kind=ip_single_p) ::x(nb),y(nbo)
    DO i=1,nb
      y(i) = x(i)*57.29577951
    ENDDO

    nbo = nb
    ma_deg_r114 = 0

  END FUNCTION ma_deg_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_rad_r114(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_rad_r114
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)*0.01745329252
    ENDDO

    nbo = nb
    ma_rad_r114 = 0

  END FUNCTION ma_rad_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_ident_r114(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_ident_r114
    REAL (kind=ip_single_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)
    ENDDO

    nbo = nb
    ma_ident_r114 = 0

  END FUNCTION ma_ident_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !     OPERATIONS (two argument)
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_add_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_add_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)+s
    ENDDO

    nbo = nb
    ma_add_r114 = 0

  END FUNCTION ma_add_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_sub_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_sub_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)-s
    ENDDO

    nbo = nb
    ma_sub_r114 = 0

  END FUNCTION ma_sub_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_subi_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_subi_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) =  s-x(i)
    ENDDO

    nbo = nb
    ma_subi_r114 = 0

  END FUNCTION ma_subi_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_mult_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_mult_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)*s
    ENDDO

    nbo = nb
    ma_mult_r114 = 0

  END FUNCTION ma_mult_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_div_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_div_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)/s
    ENDDO

    nbo = nb
    ma_div_r114 = 0

  END FUNCTION ma_div_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_divi_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_divi_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = s/x(i)
    ENDDO

    nbo = nb
    ma_divi_r114 = 0

  END FUNCTION ma_divi_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_power_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_power_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i) ** s
    ENDDO

    nbo = nb
    ma_power_r114 = 0

  END FUNCTION ma_power_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fumin_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_fumin_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = min(x(i),s)
    ENDDO

    nbo = nb
    ma_fumin_r114 = 0

  END FUNCTION ma_fumin_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fumax_r114(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_fumax_r114
    REAL (kind=ip_single_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = max(x(i),s)
    ENDDO

    nbo = nb
    ma_fumax_r114 = 0

  END FUNCTION ma_fumax_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuscat_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuscat_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ii,ipos

    ma_fuscat_r114 = 0

    DO i = 1,nbo
      y(i) = miss_val
    ENDDO

    IF (nbi <= nb) THEN

        ipos = 0
        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ind(i)) = x(ipos)
          ELSE
              IF (ind(i) > nbo) ma_fuscat_r114  = ma_fuscat_r114+1
          ENDIF
        ENDDO
        !
        !  Repeate the data if needed
        !
        IF (MINVAL(ind) < 0) THEN
            DO i = 1,nbi
              IF (ind(i) <= 0) THEN
                  DO ii=1,ABS(ind(i))-1
                    IF (ind(i+1)+ii <= nbo) THEN
                        y(ind(i+1)+ii) = y(ind(i+1))
                    ELSE
                        ma_fuscat_r114  = ma_fuscat_r114+1
                    ENDIF
                  ENDDO
              ENDIF
            ENDDO
        ENDIF
        !
    ELSE

        ma_fuscat_r114  = 1

    ENDIF

  END FUNCTION ma_fuscat_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fugath_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fugath_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ipos

    IF (nbi <= nbo) THEN

        ma_fugath_r114 = 0

        DO i=1,nbo
          y(i) = miss_val
        ENDDO

        ipos = 0
        DO i = 1,nbi
          IF (ipos+1 <= nbo) THEN
              IF (ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ipos) = x(ind(i))
              ENDIF
          ELSE
              IF (ipos+1 > nbo) ma_fugath_r114  = ma_fugath_r114+1
          ENDIF
        ENDDO

    ELSE

        ma_fugath_r114 = 1

    ENDIF

    nbo = ipos

  END FUNCTION ma_fugath_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fufill_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fufill_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ii,ipos

    ma_fufill_r114 = 0

    IF (nbi <= nb) THEN

        ipos = 0
        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ind(i)) = x(ipos)
          ELSE
              IF (ind(i) > nbo) ma_fufill_r114  = ma_fufill_r114+1
          ENDIF
        ENDDO
        !
        !  Repeate the data if needed
        !
        IF (MINVAL(ind) < 0) THEN
            DO i = 1,nbi
              IF (ind(i) <= 0) THEN
                  DO ii=1,ABS(ind(i))-1
                    IF (ind(i+1)+ii <= nbo) THEN
                        y(ind(i+1)+ii) = y(ind(i+1))
                    ELSE
                        ma_fufill_r114  = ma_fufill_r114+1
                    ENDIF
                  ENDDO
              ENDIF
            ENDDO
        ENDIF

    ELSE

        ma_fufill_r114  = 1

    ENDIF

  END FUNCTION ma_fufill_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fucoll_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fucoll_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ipos

    IF (nbi <= nbo) THEN

        ma_fucoll_r114 = 0

        ipos = 0
        DO i = 1,nbi
          !
          IF (ipos+1 <= nbo) THEN
              IF (ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ipos) = x(ind(i))
              ENDIF
          ELSE
              IF (ipos+1 > nbo) ma_fucoll_r114  = ma_fucoll_r114+1
          ENDIF
        ENDDO
        !
    ELSE
        !
        ma_fucoll_r114 = 1
        !
    ENDIF

    nbo = ipos

  END FUNCTION ma_fucoll_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuundef_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuundef_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    IF (nbi <= nbo .AND. nbo == nb) THEN

        ma_fuundef_r114 = 0

        DO i=1,nbo
          y(i) = x(i)
        ENDDO

        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
              y(ind(i)) =  miss_val
          ELSE
              IF (ind(i) > nbo) ma_fuundef_r114  = ma_fuundef_r114+1
          ENDIF
        ENDDO

    ELSE

        ma_fuundef_r114 = 1

    ENDIF

  END FUNCTION ma_fuundef_r114
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuonly_r114(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuonly_r114
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_single_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    IF (nbi <= nbo .AND. nbo == nb) THEN

        ma_fuonly_r114 = 0

        DO i=1,nbo
          y(i) = miss_val
        ENDDO

        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
              y(ind(i)) =  x(ind(i))
          ELSE
              IF (ind(i) > nbo) ma_fuonly_r114  = ma_fuonly_r114+1
          ENDIF
        ENDDO

    ELSE

        ma_fuonly_r114 = 1

    ENDIF

   END FUNCTION ma_fuonly_r114
!=
   SUBROUTINE mathop_r118(fun,nb,work_in,miss_val,nb_index,nindex,scal,nb_max,work_out)
!---------------------------------------------------------------------
!- This soubroutines gives an interface to the various operation
!- which are allowed. The interface is general enough to allow its use
!- for other cases.
!-
!- INPUT
!-
!- fun      : function to be applied to the vector of data
!- nb       : Length of input vector
!- work_in  : Input vector of data (REAL (kind=ip_double_p))
!- miss_val : The value of the missing data flag (it has to be a
!-            maximum value, in f90 : huge( a real ))
!- nb_index : Length of index vector
!- nindex   : Vector of indices
!- scal     : A scalar value for vector/scalar operations
!- nb_max   : maximum length of output vector
!-
!- OUTPUT
!-
!- nb_max   : Actual length of output variable
!- work_out : Output vector after the operation was applied
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER*7 :: fun
   INTEGER (kind=ip_intwp_p):: nb,nb_max,nb_index
   INTEGER (kind=ip_intwp_p):: nindex(nb_index)
   REAL (kind=ip_double_p) :: miss_val
   REAL (kind=ip_double_p) ::work_in(nb),scal
   REAL (kind=ip_double_p) ::work_out(nb_max)
!-
   INTEGER (kind=ip_intwp_p):: ierr
!-
   INTRINSIC SIN,COS,TAN,ASIN,ACOS,ATAN,EXP,ALOG,SQRT,ABS
!---------------------------------------------------------------------
   ierr = 0
!-
   IF (scal >= miss_val-1.) THEN
     IF (INDEX(indexfu,fun(1:LEN_TRIM(fun))) == 0) THEN
       SELECT CASE (fun)
       CASE('sin')
         ierr = ma_sin_r118(nb,work_in,nb_max,work_out)
       CASE('cos')
         ierr = ma_cos_r118(nb,work_in,nb_max,work_out)
       CASE('tan')
         ierr = ma_tan_r118(nb,work_in,nb_max,work_out)
       CASE('asin')
         ierr = ma_asin_r118(nb,work_in,nb_max,work_out)
       CASE('acos')
         ierr = ma_acos_r118(nb,work_in,nb_max,work_out)
       CASE('atan')
         ierr = ma_atan_r118(nb,work_in,nb_max,work_out)
       CASE('exp')
         ierr = ma_exp_r118(nb,work_in,nb_max,work_out)
       CASE('log')
         ierr = ma_alog_r118(nb,work_in,nb_max,work_out)
       CASE('sqrt')
         ierr = ma_sqrt_r118(nb,work_in,nb_max,work_out)
       CASE('chs')
         ierr = ma_chs_r118(nb,work_in,nb_max,work_out)
       CASE('abs')
         ierr = ma_abs_r118(nb,work_in,nb_max,work_out)
       CASE('cels')
         ierr = ma_cels_r118(nb,work_in,nb_max,work_out)
       CASE('kelv')
         ierr = ma_kelv_r118(nb,work_in,nb_max,work_out)
       CASE('deg')
         ierr = ma_deg_r118(nb,work_in,nb_max,work_out)
       CASE('rad')
         ierr = ma_rad_r118(nb,work_in,nb_max,work_out)
       CASE('ident')
         ierr = ma_ident_r118(nb,work_in,nb_max,work_out)
       CASE DEFAULT
         CALL histerr(3,"mathop",'scalar variable undefined and no indexing','but still unknown function',fun)
       END SELECT
       IF (ierr > 0) THEN
         CALL histerr(3,"mathop",'Error while executing a simple function',fun,' ')
       ENDIF
     ELSE
       SELECT CASE (fun)
       CASE('gather')
         ierr = ma_fugath_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('scatter')
         IF (nb_index > nb) THEN
           DO ierr=1,nb_max
             work_out(ierr)=miss_val
           ENDDO 
           ierr=1
         ELSE
           ierr = ma_fuscat_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
         ENDIF
       CASE('coll')
         ierr = ma_fucoll_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('fill')
         ierr = ma_fufill_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('undef')
         ierr = ma_fuundef_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE('only')
         ierr = ma_fuonly_r118(nb,work_in,nb_index,nindex,miss_val,nb_max,work_out)
       CASE DEFAULT
         CALL histerr(3,"mathop",'scalar variable undefined and indexing',&
        &              'was requested but with unknown function',fun)
       END SELECT
       IF (ierr > 0) THEN
         CALL histerr(3,"mathop_r118",'Error while executing an indexing function',fun,' ')
       ENDIF
     ENDIF
   ELSE
     SELECT CASE (fun)
     CASE('fumin')
       ierr = ma_fumin_r118(nb,work_in,scal,nb_max,work_out)
     CASE('fumax')
       ierr = ma_fumax_r118(nb,work_in,scal,nb_max,work_out)
     CASE('add')
       ierr = ma_add_r118(nb,work_in,scal,nb_max,work_out)
     CASE('subi')
       ierr = ma_subi_r118(nb,work_in,scal,nb_max,work_out)
     CASE('sub')
       ierr = ma_sub_r118(nb,work_in,scal,nb_max,work_out)
     CASE('mult')
       ierr = ma_mult_r118(nb,work_in,scal,nb_max,work_out)
     CASE('div')
       ierr = ma_div_r118(nb,work_in,scal,nb_max,work_out)
     CASE('divi')
       ierr = ma_divi_r118(nb,work_in,scal,nb_max,work_out)
     CASE('power')
       ierr = ma_power_r118(nb,work_in,scal,nb_max,work_out)
     CASE DEFAULT
       CALL histerr(3,"mathop",'Unknown operation with a scalar',fun,' ')
     END SELECT
     IF (ierr > 0) THEN
       CALL histerr(3,"mathop",'Error while executing a scalar function',fun,' ')
     ENDIF
   ENDIF
!---------------------------
 END SUBROUTINE mathop_r118
  !
  !- INCLUDE 'mafunc_r11.inc'
  !------------------------------------------------------
  !------------------------------------------------------
  !    FUNCTIONS (only one argument)
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_sin_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_sin_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = SIN(x(i))
    ENDDO

    nbo = nb
    ma_sin_r118 = 0

  END FUNCTION ma_sin_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_cos_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_cos_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = COS(x(i))
    ENDDO

    nbo = nb
    ma_cos_r118 = 0

  END FUNCTION ma_cos_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_tan_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_tan_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = TAN(x(i))
    ENDDO

    nbo = nb
    ma_tan_r118 = 0

  END FUNCTION ma_tan_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_asin_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_asin_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ASIN(x(i))
    ENDDO

    nbo = nb
    ma_asin_r118 = 0

  END FUNCTION ma_asin_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_acos_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_acos_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ACOS(x(i))
    ENDDO

    nbo = nb
    ma_acos_r118 = 0

  END FUNCTION ma_acos_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_atan_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_atan_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ATAN(x(i))
    ENDDO

    nbo = nb
    ma_atan_r118 = 0

  END FUNCTION ma_atan_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_exp_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_exp_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = EXP(x(i))
    ENDDO

    nbo = nb
    ma_exp_r118 = 0

  END FUNCTION ma_exp_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_alog_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_alog_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = log(x(i))
    ENDDO

    nbo = nb
    ma_alog_r118 = 0

  END FUNCTION ma_alog_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_sqrt_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_sqrt_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = SQRT(x(i))
    ENDDO

    nbo = nb
    ma_sqrt_r118 = 0

  END FUNCTION ma_sqrt_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_abs_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_abs_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = ABS(x(i))
    ENDDO

    nbo = nb
    ma_abs_r118 = 0

  END FUNCTION ma_abs_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_chs_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_chs_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)*(-1.)
    ENDDO

    nbo = nb
    ma_chs_r118 = 0

  END FUNCTION ma_chs_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_cels_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_cels_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)-273.15
    ENDDO

    nbo = nb
    ma_cels_r118 = 0


  END FUNCTION ma_cels_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_kelv_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_kelv_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)+273.15
    ENDDO

    nbo = nb
    ma_kelv_r118 = 0

  END FUNCTION ma_kelv_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_deg_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_deg_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)
    DO i=1,nb
      y(i) = x(i)*57.29577951
    ENDDO

    nbo = nb
    ma_deg_r118 = 0

  END FUNCTION ma_deg_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_rad_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_rad_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)*0.01745329252
    ENDDO

    nbo = nb
    ma_rad_r118 = 0

  END FUNCTION ma_rad_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_ident_r118(nb,x,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,i,ma_ident_r118
    REAL (kind=ip_double_p) ::x(nb),y(nbo)

    DO i=1,nb
      y(i) = x(i)
    ENDDO

    nbo = nb
    ma_ident_r118 = 0

  END FUNCTION ma_ident_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !     OPERATIONS (two argument)
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_add_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_add_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)+s
    ENDDO

    nbo = nb
    ma_add_r118 = 0

  END FUNCTION ma_add_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_sub_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_sub_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)-s
    ENDDO

    nbo = nb
    ma_sub_r118 = 0

  END FUNCTION ma_sub_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_subi_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_subi_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) =  s-x(i)
    ENDDO

    nbo = nb
    ma_subi_r118 = 0

  END FUNCTION ma_subi_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_mult_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_mult_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)*s
    ENDDO

    nbo = nb
    ma_mult_r118 = 0

  END FUNCTION ma_mult_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_div_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_div_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i)/s
    ENDDO

    nbo = nb
    ma_div_r118 = 0

  END FUNCTION ma_div_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_divi_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_divi_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = s/x(i)
    ENDDO

    nbo = nb
    ma_divi_r118 = 0

  END FUNCTION ma_divi_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_power_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_power_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = x(i) ** s
    ENDDO

    nbo = nb
    ma_power_r118 = 0

  END FUNCTION ma_power_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fumin_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_fumin_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = min(x(i),s)
    ENDDO

    nbo = nb
    ma_fumin_r118 = 0

  END FUNCTION ma_fumin_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fumax_r118(nb,x,s,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,ma_fumax_r118
    REAL (kind=ip_double_p) ::x(nb),s,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    DO i = 1,nb
      y(i) = max(x(i),s)
    ENDDO

    nbo = nb
    ma_fumax_r118 = 0

  END FUNCTION ma_fumax_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuscat_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuscat_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ii,ipos

    ma_fuscat_r118 = 0

    DO i = 1,nbo
      y(i) = miss_val
    ENDDO

    IF (nbi <= nb) THEN

        ipos = 0
        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ind(i)) = x(ipos)
          ELSE
              IF (ind(i) > nbo) ma_fuscat_r118  = ma_fuscat_r118+1
          ENDIF
        ENDDO
        !
        !  Repeate the data if needed
        !
        IF (MINVAL(ind) < 0) THEN
            DO i = 1,nbi
              IF (ind(i) <= 0) THEN
                  DO ii=1,ABS(ind(i))-1
                    IF (ind(i+1)+ii <= nbo) THEN
                        y(ind(i+1)+ii) = y(ind(i+1))
                    ELSE
                        ma_fuscat_r118  = ma_fuscat_r118+1
                    ENDIF
                  ENDDO
              ENDIF
            ENDDO
        ENDIF
        !
    ELSE

        ma_fuscat_r118  = 1

    ENDIF

  END FUNCTION ma_fuscat_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fugath_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fugath_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ipos

    IF (nbi <= nbo) THEN

        ma_fugath_r118 = 0

        DO i=1,nbo
          y(i) = miss_val
        ENDDO

        ipos = 0
        DO i = 1,nbi
          IF (ipos+1 <= nbo) THEN
              IF (ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ipos) = x(ind(i))
              ENDIF
          ELSE
              IF (ipos+1 > nbo) ma_fugath_r118  = ma_fugath_r118+1
          ENDIF
        ENDDO

    ELSE

        ma_fugath_r118 = 1

    ENDIF

    nbo = ipos

  END FUNCTION ma_fugath_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fufill_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fufill_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ii,ipos

    ma_fufill_r118 = 0

    IF (nbi <= nb) THEN

        ipos = 0
        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ind(i)) = x(ipos)
          ELSE
              IF (ind(i) > nbo) ma_fufill_r118  = ma_fufill_r118+1
          ENDIF
        ENDDO
        !
        !  Repeate the data if needed
        !
        IF (MINVAL(ind) < 0) THEN
            DO i = 1,nbi
              IF (ind(i) <= 0) THEN
                  DO ii=1,ABS(ind(i))-1
                    IF (ind(i+1)+ii <= nbo) THEN
                        y(ind(i+1)+ii) = y(ind(i+1))
                    ELSE
                        ma_fufill_r118  = ma_fufill_r118+1
                    ENDIF
                  ENDDO
              ENDIF
            ENDDO
        ENDIF

    ELSE

        ma_fufill_r118  = 1

    ENDIF

  END FUNCTION ma_fufill_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fucoll_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fucoll_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i,ipos

    IF (nbi <= nbo) THEN

        ma_fucoll_r118 = 0

        ipos = 0
        DO i = 1,nbi
          !
          IF (ipos+1 <= nbo) THEN
              IF (ind(i) > 0) THEN
                  ipos = ipos+1
                  y(ipos) = x(ind(i))
              ENDIF
          ELSE
              IF (ipos+1 > nbo) ma_fucoll_r118  = ma_fucoll_r118+1
          ENDIF
        ENDDO
        !
    ELSE
        !
        ma_fucoll_r118 = 1
        !
    ENDIF

    nbo = ipos

  END FUNCTION ma_fucoll_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuundef_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuundef_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    IF (nbi <= nbo .AND. nbo == nb) THEN

        ma_fuundef_r118 = 0

        DO i=1,nbo
          y(i) = x(i)
        ENDDO

        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
              y(ind(i)) =  miss_val
          ELSE
              IF (ind(i) > nbo) ma_fuundef_r118  = ma_fuundef_r118+1
          ENDIF
        ENDDO

    ELSE

        ma_fuundef_r118 = 1

    ENDIF

  END FUNCTION ma_fuundef_r118
  !------------------------------------------------------
  !------------------------------------------------------
  !------------------------------------------------------
  FUNCTION ma_fuonly_r118(nb,x,nbi,ind,miss_val,nbo,y)
   USE mod_kinds_model

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p):: nb,nbo,nbi,ma_fuonly_r118
    INTEGER (kind=ip_intwp_p):: ind(nbi)
    REAL (kind=ip_double_p) ::x(nb),miss_val,y(nbo)

    INTEGER (kind=ip_intwp_p):: i

    IF (nbi <= nbo .AND. nbo == nb) THEN

        ma_fuonly_r118 = 0

        DO i=1,nbo
          y(i) = miss_val
        ENDDO

        DO i = 1,nbi
          IF (ind(i) <= nbo .AND. ind(i) > 0) THEN
              y(ind(i)) =  x(ind(i))
          ELSE
              IF (ind(i) > nbo) ma_fuonly_r118  = ma_fuonly_r118+1
          ENDIF
        ENDDO

    ELSE

        ma_fuonly_r118 = 1

    ENDIF

   END FUNCTION ma_fuonly_r118
!=
   SUBROUTINE moycum_r4 (opp,np,px,py,pwx)
!---------------------------------------------------------------------
!- Does time operations
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   CHARACTER(LEN=7) :: opp
   INTEGER (kind=ip_intwp_p) :: np
   REAL (kind=ip_single_p),DIMENSION(:) :: px,py
   INTEGER (kind=ip_intwp_p) :: pwx
!---------------------------------------------------------------------
   IF (pwx /= 0) THEN
     IF      (opp == 'ave') THEN
       px(1:np)=(px(1:np)*pwx+py(1:np))/REAL(pwx+1,kind=ip_single_p)
     ELSE IF (opp == 't_sum') THEN
       px(1:np)=px(1:np)+py(1:np)
     ELSE IF ( (opp == 'l_min').OR.(opp == 't_min') ) THEN
       px(1:np)=MIN(px(1:np),py(1:np))
     ELSE IF ( (opp == 'l_max').OR.(opp == 't_max') ) THEN
       px(1:np)=MAX(px(1:np),py(1:np))
     ELSE
       CALL histerr(3,"moycum",'Unknown time operation',opp,' ')
     ENDIF
   ELSE
     IF      (opp == 'l_min') THEN
       px(1:np)=MIN(px(1:np),py(1:np))
     ELSE IF (opp == 'l_max') THEN
       px(1:np)=MAX(px(1:np),py(1:np))
     ELSE
       px(1:np)=py(1:np)
     ENDIF
   ENDIF
!-----------------------
 END SUBROUTINE moycum_r4
!=
   SUBROUTINE moycum_r8 (opp,np,px,py,pwx)
!---------------------------------------------------------------------
!- Does time operations
!---------------------------------------------------------------------
    USE mod_kinds_model
    IMPLICIT NONE
!-
   CHARACTER(LEN=7) :: opp
   INTEGER (kind=ip_intwp_p) :: np
   REAL (kind=ip_double_p),DIMENSION(:) :: px,py
   INTEGER (kind=ip_intwp_p) :: pwx
!---------------------------------------------------------------------
   IF (pwx /= 0) THEN
     IF      (opp == 'ave') THEN
       px(1:np)=(px(1:np)*pwx+py(1:np))/REAL(pwx+1,kind=ip_double_p)
     ELSE IF (opp == 't_sum') THEN
       px(1:np)=px(1:np)+py(1:np)
     ELSE IF ( (opp == 'l_min').OR.(opp == 't_min') ) THEN
       px(1:np)=MIN(px(1:np),py(1:np))
     ELSE IF ( (opp == 'l_max').OR.(opp == 't_max') ) THEN
       px(1:np)=MAX(px(1:np),py(1:np))
     ELSE
       CALL histerr(3,"moycum",'Unknown time operation',opp,' ')
     ENDIF
   ELSE
     IF      (opp == 'l_min') THEN
       px(1:np)=MIN(px(1:np),py(1:np))
     ELSE IF (opp == 'l_max') THEN
       px(1:np)=MAX(px(1:np),py(1:np))
     ELSE
       px(1:np)=py(1:np)
     ENDIF
   ENDIF
!-----------------------
 END SUBROUTINE moycum_r8
!=
!-----------------
END MODULE mathelp_psmile
