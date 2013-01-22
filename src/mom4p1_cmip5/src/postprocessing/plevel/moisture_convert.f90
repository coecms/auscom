
module moisture_convert_mod

!-----------------------------------------------------------------------

use       constants_mod, only: rdgas, rvgas
use  sat_vapor_pres_mod, only: escomp

implicit none
private

public  sphum_to_mixrat, mixrat_to_sphum,  &
            e_to_mixrat,      e_to_sphum,  &
           mixrat_to_rh,     sphum_to_rh,  &
           rh_to_mixrat,     rh_to_sphum

!-----------------------------------------------------------------------

interface sphum_to_mixrat
    module procedure sphum_to_mixrat_0d, sphum_to_mixrat_3d
end interface

interface mixrat_to_sphum
    module procedure mixrat_to_sphum_0d, mixrat_to_sphum_3d
end interface

interface e_to_mixrat
    module procedure e_to_mixrat_0d, e_to_mixrat_3d
end interface

interface e_to_sphum
    module procedure e_to_sphum_0d, e_to_sphum_3d
end interface

interface mixrat_to_rh
    module procedure mixrat_to_rh_0d, mixrat_to_rh_3d
end interface

interface sphum_to_rh
    module procedure sphum_to_rh_0d, sphum_to_rh_3d
end interface

interface rh_to_mixrat
    module procedure rh_to_mixrat_0d, rh_to_mixrat_3d
end interface

interface rh_to_sphum
    module procedure rh_to_sphum_0d, rh_to_sphum_3d
end interface

!-----------------------------------------------------------------------

real, parameter :: eps = rdgas/rvgas,  one_eps = 1.-eps

contains

!#######################################################################

  function sphum_to_mixrat_0d (sphum) result (mixrat)

    real, intent(in) :: sphum
    real             :: mixrat

    mixrat = sphum / (1.0 - sphum)

  end function sphum_to_mixrat_0d

!-----------------------------------------------------------------------

  function sphum_to_mixrat_3d (sphum) result (mixrat)

    real, intent(in) :: sphum(:,:,:)
    real, dimension(size(sphum,1),size(sphum,2),size(sphum,3)) :: mixrat

    mixrat = sphum / (1.0 - sphum)

  end function sphum_to_mixrat_3d

!#######################################################################

  function mixrat_to_sphum_0d (mixrat) result (sphum)

    real, intent(in) :: mixrat
    real             :: sphum

    sphum = mixrat / (1.0 + mixrat)

  end function mixrat_to_sphum_0d

!-----------------------------------------------------------------------

  function mixrat_to_sphum_3d (mixrat) result (sphum)

    real, intent(in) :: mixrat(:,:,:)
    real, dimension(size(mixrat,1),size(mixrat,2),size(mixrat,3)) ::  &
            sphum

    sphum = mixrat / (1.0 + mixrat)

  end function mixrat_to_sphum_3d

!#######################################################################

  function e_to_mixrat_0d (e, pres) result (mixrat)

    real, intent(in) :: e, pres
    real             :: mixrat

    mixrat = eps * e / (pres - e )

  end function e_to_mixrat_0d

!-----------------------------------------------------------------------

  function e_to_mixrat_3d (e, pres) result (mixrat)

    real, intent(in), dimension(:,:,:) :: e, pres
    real, dimension(size(e,1),size(e,2),size(e,3)) :: mixrat

    mixrat = eps * e / (pres - e )

  end function e_to_mixrat_3d

!#######################################################################

  function e_to_sphum_0d (e, pres) result (sphum)

    real, intent(in) :: e, pres
    real             :: sphum

    sphum = eps * e / (pres - one_eps * e )

  end function e_to_sphum_0d

!-----------------------------------------------------------------------

  function e_to_sphum_3d (e, pres) result (sphum)

    real, intent(in), dimension(:,:,:) :: e, pres
    real, dimension(size(e,1),size(e,2),size(e,3)) :: sphum

    sphum = eps * e / (pres - one_eps * e )

  end function e_to_sphum_3d

!#######################################################################

  function mixrat_to_rh_0d (mixrat, temp, pres) result (rh)

    real, intent(in) :: mixrat, temp, pres
    real             :: rh, esat, mixrat_sat

    call escomp (temp, esat)
    mixrat_sat = e_to_mixrat (esat, pres)

    rh = mixrat / mixrat_sat

  end function mixrat_to_rh_0d

!-----------------------------------------------------------------------

  function mixrat_to_rh_3d (mixrat, temp, pres) result (rh)

    real, intent(in), dimension(:,:,:) :: mixrat, temp, pres
    real, dimension(size(temp,1),size(temp,2),size(temp,3)) ::  &
            rh, esat, mixrat_sat

    call escomp (temp, esat)
    mixrat_sat = e_to_mixrat (esat, pres)

    rh = mixrat / mixrat_sat

  end function mixrat_to_rh_3d

!#######################################################################

  function sphum_to_rh_0d (sphum, temp, pres) result (rh)

    real, intent(in) :: sphum, temp, pres
    real  ::  rh, mixrat

    mixrat = sphum_to_mixrat (sphum)
    rh = mixrat_to_rh (mixrat, temp, pres)

  end function sphum_to_rh_0d

!-----------------------------------------------------------------------

  function sphum_to_rh_3d (sphum, temp, pres) result (rh)

    real, intent(in), dimension(:,:,:) :: sphum, temp, pres
    real, dimension(size(temp,1),size(temp,2),size(temp,3)) ::  &
            rh, mixrat

    mixrat = sphum_to_mixrat (sphum)
    rh = mixrat_to_rh (mixrat, temp, pres)

  end function sphum_to_rh_3d

!#######################################################################

  function rh_to_mixrat_0d (rh, temp, pres) result (mixrat)

    real, intent(in) :: rh, temp, pres
    real             ::  mixrat, esat, mixrat_sat

    call escomp (temp, esat)
    mixrat_sat = e_to_mixrat (esat, pres)

    mixrat = mixrat_sat * rh

  end function rh_to_mixrat_0d

!-----------------------------------------------------------------------

  function rh_to_mixrat_3d (rh, temp, pres) result (mixrat)

    real, intent(in), dimension(:,:,:) :: rh, temp, pres
    real, dimension(size(temp,1),size(temp,2),size(temp,3)) ::  &
            mixrat, esat, mixrat_sat

    call escomp (temp, esat)
    mixrat_sat = e_to_mixrat (esat, pres)

    mixrat = mixrat_sat * rh

  end function rh_to_mixrat_3d

!#######################################################################

  function rh_to_sphum_0d (rh, temp, pres) result (sphum)

    real, intent(in) :: rh, temp, pres
    real             ::  sphum, mixrat

    mixrat = rh_to_mixrat (rh, temp, pres)
    sphum  = mixrat_to_sphum (mixrat)

  end function rh_to_sphum_0d

!-----------------------------------------------------------------------

  function rh_to_sphum_3d (rh, temp, pres) result (sphum)

    real, intent(in), dimension(:,:,:) :: rh, temp, pres
    real, dimension(size(temp,1),size(temp,2),size(temp,3)) ::  &
            sphum, mixrat

    mixrat = rh_to_mixrat (rh, temp, pres)
    sphum  = mixrat_to_sphum (mixrat)

  end function rh_to_sphum_3d

!#######################################################################

end module moisture_convert_mod

