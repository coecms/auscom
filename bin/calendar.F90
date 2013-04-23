!------------------------------------------------------------------------------
! Calendar
!  - This program was created as here document - 
!------------------------------------------------------------------------------
PROGRAM calendar
IMPLICIT NONE
!------------------------------------------------------------------------------
! Input parameters
!------------------------------------------------------------------------------
INTEGER  inidate       ! initial date of the experiment
INTEGER  date          ! initial date of the run
INTEGER  nyear         ! number of years to be simulated within the run
INTEGER  nmonth        ! number of months to be simulated within the run
INTEGER  nday          ! number of days to be simulated within the run
INTEGER  calendar_type ! calendar type: 0 - no leap years
                       !                1 - realistic
                       !               'n'- equal months of n days
!------------------------------------------------------------------------------
! Output parameters
!------------------------------------------------------------------------------
INTEGER  nextdate      ! initial date of the next run
INTEGER  prevdate      ! final date of the previous run
INTEGER  enddate       ! final date of this run
INTEGER  previnidate   ! day before the initial date of the experiment
INTEGER  days_in_run   ! duration of the run in days
INTEGER  days_since_start ! number of days since the beginning of the exp.
INTEGER  date_in_days     ! date in days (days since 00010101)

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
INTEGER iniyear, year,  nextyear,  prevyear,  endyear, previniyear
INTEGER inimonth, month, nextmonth, prevmonth, endmonth, previnimonth
INTEGER iniday, day,   nextday,   prevday,   endday, previniday
INTEGER days_per_month ! function to calculate the number of days per month
INTEGER days_per_year  ! function to calculate the number of days the surrent year
INTEGER days_between   ! function to calculate the number of days between date1
                       ! and date2
INTEGER date0          ! initial date: 00010101

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! read input
!-----------
read (5,*) inidate, date, nyear, nmonth, nday, calendar_type

! initial date of the experiment
!-------------------------------
iniyear = inidate / 10000
inimonth = (inidate - iniyear*10000) / 100
iniday = inidate - iniyear*10000 - inimonth*100

! initial date of the run
!------------------------
year = date / 10000
month = (date - year*10000) / 100
day = date - year*10000 - month*100

!------------------------------------------------------------------------------
! calculate initial date of the next run
!------------------------------------------------------------------------------

nextday = day + nday
nextmonth = month + nmonth
nextyear = year + nyear

IF ((calendar_type == 0) .OR. (calendar_type == 1)) THEN

   IF ((nday /= 0) .AND. ((nmonth /= 0) .OR. (nyear /= 0)) .OR. &
       (nmonth /= 0) .AND. ((nday /= 0) .OR. (nyear /= 0))) THEN
      PRINT*, "Job length not allowed with calendar_type ", calendar_type
      PRINT*, &
         "Choose either number of years or number of days or number of months"
      STOP
   ELSE IF (nmonth /= 0) THEN
      DO WHILE (nextmonth > 12)
         nextmonth = nextmonth - 12
         nextyear = nextyear + 1
      END DO  
      DO WHILE (nextday > days_per_month(nextmonth,nextyear,calendar_type))
         nextday = nextday - days_per_month(nextmonth,nextyear,calendar_type)
         nextmonth = nextmonth + 1
      END DO
      DO WHILE (nextmonth > 12)
         nextmonth = nextmonth - 12
         nextyear = nextyear + 1
      END DO  
!20110321: Terrt O'kane/Russ Fielder suggested this part causes problem when AusCOM
!          runs cross yeears in nday/=0 runs. They commented it out......
!	   I will investigate it when not busy.....................................
   ELSE IF (nday /= 0) THEN
      DO WHILE (nextday > days_per_month(nextmonth,nextyear,calendar_type))
         nextday = nextday - days_per_month(nextmonth,nextyear,calendar_type)
         nextmonth = nextmonth + 1
      END DO
!20110321-end
   END IF

ELSE                   ! month of equal length
                       ! (calendar_type gives the number of days per month)
   DO WHILE (nextday > calendar_type)
      nextday = nextday - calendar_type
      nextmonth = nextmonth + 1
   END DO
   DO WHILE (nextmonth > 12)
      nextmonth = nextmonth - 12
      nextyear = nextyear + 1
   END DO

END IF

!------------------------------------------------------------------------------
! calculate the date before the initial date (restart date of initial run)
!------------------------------------------------------------------------------
previniday = iniday - 1
previnimonth = inimonth
previniyear = iniyear
IF (previniday == 0) THEN
   previnimonth = previnimonth - 1
   IF (previnimonth == 0) THEN
      previnimonth = 12
      previniyear = previniyear - 1
   ENDIF
   previniday = days_per_month (previnimonth, previniyear, calendar_type)
ENDIF

!------------------------------------------------------------------------------
! calculate final date of the previous run
!------------------------------------------------------------------------------
prevday = day - 1
prevmonth = month
prevyear = year
IF (prevday == 0) THEN
   prevmonth = month - 1
   IF (prevmonth == 0) THEN
      prevmonth = 12
      prevyear = prevyear - 1
   ENDIF
   prevday = days_per_month (prevmonth, prevyear, calendar_type)
ENDIF

!------------------------------------------------------------------------------
! calculate final date of the run
!------------------------------------------------------------------------------
endday = nextday - 1
endmonth = nextmonth
endyear = nextyear
IF (endday == 0) THEN
   endmonth = nextmonth - 1
   IF (endmonth == 0) THEN
      endmonth = 12
      endyear = nextyear - 1
   ENDIF
   endday = days_per_month (endmonth, endyear, calendar_type)
ENDIF

prevdate = prevyear*10000 + prevmonth*100 + prevday
enddate  = endyear *10000 + endmonth *100 + endday
nextdate = nextyear*10000 + nextmonth*100 + nextday
previnidate = previniyear*10000 + previnimonth*100 + previniday

!------------------------------------------------------------------------------
! calculate length of the run (in days)
!------------------------------------------------------------------------------

days_in_run = days_between (date, enddate, calendar_type)

!------------------------------------------------------------------------------
! calculate the number of days since the beginning of the experiment
!------------------------------------------------------------------------------

days_since_start = days_between (inidate, date, calendar_type)

!------------------------------------------------------------------------------
! calculate the date in days
!------------------------------------------------------------------------------
date0 = 00010101
date_in_days = days_between (date0, date, calendar_type)

!------------------------------------------------------------------------------

WRITE(UNIT=6,FMT='(4(1x,i8.8),3(1X,i8),1X,i3)')  prevdate, enddate, nextdate, previnidate,  &
         days_in_run, days_since_start, date_in_days, days_per_year(endyear,calendar_type) 


STOP
END PROGRAM calendar

!------------------------------------------------------------------------------
INTEGER FUNCTION days_per_month (mon, yr, calendar)

!------------------------------------------------------------------------------
IMPLICIT NONE
!------------------------------------------------------------------------------
INTEGER mon
INTEGER yr
INTEGER calendar
!------------------------------------------------------------------------------
IF (calendar == 0 .OR. calendar == 1) THEN
   IF (mon == 1 .OR. mon == 3 .OR. mon == 5 .OR. mon == 7 .OR. &
       mon == 8 .OR. mon == 10 .OR. mon == 12) THEN
      days_per_month = 31
   ELSE IF (mon == 4 .OR. mon == 6 .OR. mon == 9 .OR. mon == 11) THEN
      days_per_month = 30
   ELSE
      days_per_month = 28
      IF (calendar == 1) THEN
         IF(MOD(yr,  4).EQ.0) days_per_month = 29
         IF(MOD(yr,100).EQ.0) days_per_month = 28
         IF(MOD(yr,400).EQ.0) days_per_month = 29
      ENDIF
   ENDIF
ELSE
   days_per_month = calendar
END IF

RETURN
END FUNCTION days_per_month

!------------------------------------------------------------------------------
INTEGER FUNCTION days_per_year (yr,calendar)

!------------------------------------------------------------------------------
IMPLICIT NONE
!------------------------------------------------------------------------------
INTEGER yr
INTEGER calendar
!------------------------------------------------------------------------------
IF (calendar == 0) THEN
   days_per_year = 365
ELSE IF (calendar == 1) THEN
   days_per_year = 365
   IF (calendar == 1) THEN
      IF(MOD(yr,  4).EQ.0) days_per_year = 366
      IF(MOD(yr,100).EQ.0) days_per_year = 365
      IF(MOD(yr,400).EQ.0) days_per_year = 366
   ENDIF
ELSE
   days_per_year = calendar * 12
END IF

RETURN
END FUNCTION days_per_year

!------------------------------------------------------------------------------
INTEGER FUNCTION days_between (date1, date2, calendar)

!------------------------------------------------------------------------------
IMPLICIT NONE
!------------------------------------------------------------------------------
INTEGER date1, year1, month1, day1
INTEGER date2, year2, month2, day2
INTEGER calendar
INTEGER mm, yy
INTEGER days_per_month ! function to calculate the number of days per month
INTEGER days_per_year  ! function to calculate the number of days per year
!------------------------------------------------------------------------------

! calculate year, month and day from the date
!--------------------------------------------
year1 = date1 / 10000
month1 = (date1 - year1*10000) / 100
day1 = date1 - year1*10000 - month1*100

year2 = date2 / 10000
month2 = (date2 - year2*10000) / 100
day2 = date2 - year2*10000 - month2*100

! calculate the number of days between date1 and date2
!-----------------------------------------------------
IF (year1 /= year2) THEN
! a) days until the end of year1
   days_between = days_per_month(month1, year1, calendar) - day1 + 1
   DO mm = month1+1, 12
      days_between = days_between + days_per_month(mm, year1, calendar)
   END DO
! b) days of the years between year1 and year2
   DO yy = year1+1, year2-1
      days_between = days_between + days_per_year(yy, calendar)
   END DO
! c) days in year2
   DO mm = 1, month2-1
      days_between = days_between + days_per_month(mm, year2, calendar)
   END DO
   days_between = days_between + day2
ELSE
   IF (month1 /= month2) THEN
      days_between = days_per_month(month1,year1,calendar) - day1 + 1
      DO mm = month1+1, month2-1
         days_between = days_between + days_per_month(mm, year1, calendar)
      END DO
      days_between = days_between + day2
    ELSE
      days_between = day2 - day1 + 1
    END IF
END IF

RETURN
END FUNCTION days_between
!------------------------------------------------------------------------------

