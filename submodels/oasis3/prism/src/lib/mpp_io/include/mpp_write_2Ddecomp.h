#ifdef use_CRI_pointers
#undef use_local_CRI_pointers
#endif
subroutine MPP_WRITE_2DDECOMP_1D_( unit, field, domain, data, tstamp,blockid )
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(inout) :: domain
      MPP_TYPE_, intent(inout) :: data(:)
      real(DOUBLE_KIND), intent(in), optional :: tstamp
      integer,intent(in),optional::blockid
      integer::il_xbegin,il_xend,il_ybegin,il_yend,ij
      MPP_TYPE_,allocatable :: data3D(:,:,:)
      call mpp_get_compute_domain(domain &
                                ,xbegin=il_xbegin,xend=il_xend &
                                ,ybegin=il_ybegin,yend=il_yend)
      allocate(data3D(1:il_xend-il_xbegin+1,1:il_yend-il_ybegin+1,1:1))
      data3D = RESHAPE( data, SHAPE(data3D) )
!      write(0,*)'MPP_WRITE_2DDECOMP_1D',size(data3D,1),size(data3D,2)
      if(PRESENT(blockid)) then
      call mpp_write( unit, field, domain, data3D, tstamp ,blockid)
      else
      call mpp_write( unit, field, domain, data3D, tstamp )
      endif
      data   = RESHAPE( data3D, SHAPE(data) )
      deallocate(data3D)

      return
    end subroutine MPP_WRITE_2DDECOMP_1D_

    subroutine MPP_WRITE_2DDECOMP_2D_( unit, field, domain, data, tstamp, blockid )
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(inout) :: domain
      MPP_TYPE_, intent(inout) :: data(:,:)
      real(DOUBLE_KIND), intent(in), optional :: tstamp
      integer,intent(in),optional::blockid
      MPP_TYPE_ :: data3D(size(data,1),size(data,2),1)
#ifdef use_local_CRI_pointers
      pointer( ptr, data3D )
      ptr = LOC(data)
      if(PRESENT(blockid)) then
      call mpp_write( unit, field, domain, data3D, tstamp ,blockid)
      else
      call mpp_write( unit, field, domain, data3D, tstamp )
      endif
#else
      data3D = RESHAPE( data, SHAPE(data3D) )
      if(PRESENT(blockid)) then
      call mpp_write( unit, field, domain, data3D, tstamp ,blockid)
      else
      call mpp_write( unit, field, domain, data3D, tstamp )
      endif
      data   = RESHAPE( data3D, SHAPE(data) )
#endif
      return
    end subroutine MPP_WRITE_2DDECOMP_2D_

    subroutine MPP_WRITE_2DDECOMP_3D_( unit, field, domain, data, tstamp ,blockid)
!mpp_write writes <data> which has the domain decomposition <domain>
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(inout) :: domain !must have intent(out) as well because active domain might be reset
      MPP_TYPE_, intent(inout) :: data(:,:,:)
      real(DOUBLE_KIND), intent(in), optional :: tstamp
      integer,intent(in),optional::blockid
!cdata is used to store compute domain as contiguous data
!gdata is used to globalize data for multi-PE single-threaded I/O
      MPP_TYPE_, allocatable, dimension(:,:,:) :: cdata, gdata
!NEW: data may be on compute OR data domain
      logical :: data_has_halos, halos_are_global, x_is_global, y_is_global
      integer :: is, ie, js, je, isd, ied, jsd, jed, isg, ieg, jsg, jeg
!rr   integer :: mypelist(4)

!      write(0,*)'MPP_WRITE_2DDECOMP_3D',size(data,1),size(data,2),size(data,3)
      if( .NOT.module_is_initialized )call mpp_error( FATAL, 'MPP_WRITE: must first call mpp_io_init.' )
      if( .NOT.mpp_file(unit)%opened )call mpp_error( FATAL, 'MPP_WRITE: invalid unit number.' )
!      write(0,*)'MPP_WRITE_2DDECOMP_3D','calling mpp_get_compute_domain'
      call mpp_get_compute_domain( domain, is,  ie,  js,  je  )
!      write(0,*)'MPP_WRITE_2DDECOMP_3D','calling mpp_get_data_domain'
      call mpp_get_data_domain   ( domain, isd, ied, jsd, jed, x_is_global=x_is_global, y_is_global=y_is_global )
!      write(0,*)'MPP_WRITE_2DDECOMP_3D','calling mpp_get_global_domain'
      call mpp_get_global_domain ( domain, isg, ieg, jsg, jeg )
!      write(0,*)'MPP_WRITE_2DDECOMP_3D','checking for halos '
      if(debug) &
      print*,'MPP_WRITE_2DDECOMP_3D:',isd, ied, jsd, jed,is,  ie,  js,  je,'|' &
            , size(data,1),size(data,2)
      if( size(data,1).EQ.ie-is+1 .AND. size(data,2).EQ.je-js+1 )then
          data_has_halos = .FALSE.
      else if( size(data,1).EQ.ied-isd+1 .AND. size(data,2).EQ.jed-jsd+1 )then
          data_has_halos = .TRUE.
      else
          call mpp_error( FATAL, 'MPP_WRITE: data must be either on compute domain or data domain.' )
      end if
      halos_are_global = x_is_global .AND. y_is_global
      if(debug) &
       write(0,*)'MPP_WRITE_2DDECOMP_3D','checked for halos ',halos_are_global,npes
      if( npes.GT.1 .AND. mpp_file(unit)%threading.EQ.MPP_SINGLE )then
      if(debug) &
       write(0,*)'MPP_WRITE_2DDECOMP_3D','npes.GT.1','threading.EQ.MPP_SINGLE'
          if( halos_are_global )then
              call mpp_update_domains( data, domain )
!all non-0 PEs have passed their data to PE 0 and may now exit
!rv,sgi              if( pe.NE.0 )return
              if( pe.NE.mpp_root_pe() )return
                if(PRESENT(blockid)) then
                  call write_record_b( unit, field, size(data), data, tstamp,block_id=blockid)
                else
                  call write_record( unit, field, size(data), data, tstamp )
                endif
          else
!put field onto global domain
      if(debug) &
               write(0,*)'MPP_WRITE_2DDECOMP_3D','allocating gdata'
              allocate( gdata(isg:ieg,jsg:jeg,size(data,3)) )
!              write(0,*)'MPP_WRITE_2DDECOMP_3D','calling mpp_global_field'
              call mpp_global_field( domain, data, gdata )
      if(debug) &
               write(0,*)'MPP_WRITE_2DDECOMP_3D','called mpp_global_field',pe
!rr                 call mpp_get_current_pelist(mypelist)
!rr      if(debug) &
!rr               write(0,*)'MPP_WRITE_2DDECOMP_3D','mypelist',mypelist,'|' &
!rr                                    ,mpp_root_pe()

!all non-0 PEs have passed their data to PE 0 and may now exit
!rv,sgi              if( pe.NE.0 )return
              if( pe.NE. mpp_root_pe() )return
              if(PRESENT(blockid)) then
                call write_record_b( unit, field, size(gdata), gdata, tstamp,block_id=blockid )
              else
                call write_record( unit, field, size(gdata), gdata, tstamp )
              endif
          end if
      else if( data_has_halos )then
      if(debug) &
           write(0,*)'MPP_WRITE_2DDECOMP_3D','data_has_halos '
!store compute domain as contiguous data and pass to write_record
          allocate( cdata(is:ie,js:je,size(data,3)) )
          cdata(:,:,:) = data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:)
          if(PRESENT(blockid)) then
            call write_record_b( unit, field, size(cdata), cdata, tstamp, domain ,block_id=blockid)
          else
            call write_record( unit, field, size(cdata), cdata, tstamp, domain )
          endif
      if(debug) &
          write(0,*)'MPP_WRITE_2DDECOMP_3D','done with data_has_halos '
      else
      if(debug) &
           write(0,*)'MPP_WRITE_2DDECOMP_3D','contigous ',size(data)
!data is already contiguous
          if(PRESENT(blockid)) then
            call write_record_b( unit, field, size(data), data, tstamp, domain,block_id=blockid )
          else
            call write_record( unit, field, size(data), data, tstamp, domain )
          endif
      if(debug) &
           write(0,*)'MPP_WRITE_2DDECOMP_3D','done with contigous '
      end if
      return
    end subroutine MPP_WRITE_2DDECOMP_3D_

    subroutine MPP_WRITE_2DDECOMP_4D_( unit, field, domain, data, tstamp ,blockid)
!mpp_write writes <data> which has the domain decomposition <domain>
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(inout) :: domain !must have intent(out) as well because active domain might be reset
      MPP_TYPE_, intent(inout) :: data(:,:,:,:)
      real(DOUBLE_KIND), intent(in), optional :: tstamp
      integer,intent(in),optional::blockid
!cdata is used to store compute domain as contiguous data
!gdata is used to globalize data for multi-PE single-threaded I/O
      MPP_TYPE_, allocatable, dimension(:,:,:,:) :: cdata, gdata
!NEW: data may be on compute OR data domain
      logical :: data_has_halos, halos_are_global, x_is_global, y_is_global
      integer :: is, ie, js, je, isd, ied, jsd, jed, isg, ieg, jsg, jeg

!      write(0,*)'MPP_WRITE_2DDECOMP_4D',size(data,1),size(data,2),size(data,3),size(data,4)
      if( .NOT.module_is_initialized )call mpp_error( FATAL, 'MPP_WRITE: must first call mpp_io_init.' )
      if( .NOT.mpp_file(unit)%opened )call mpp_error( FATAL, 'MPP_WRITE: invalid unit number.' )
!      write(0,*)'MPP_WRITE_2DDECOMP_4D','calling mpp_get_compute_domain'
      call mpp_get_compute_domain( domain, is,  ie,  js,  je  )
!      write(0,*)'MPP_WRITE_2DDECOMP_4D','calling mpp_get_data_domain'
      call mpp_get_data_domain   ( domain, isd, ied, jsd, jed, x_is_global=x_is_global, y_is_global=y_is_global )
!      write(0,*)'MPP_WRITE_2DDECOMP_4D','calling mpp_get_global_domain'
      call mpp_get_global_domain ( domain, isg, ieg, jsg, jeg )
!      write(0,*)'MPP_WRITE_2DDECOMP_3D','checking for halos '
      if( size(data,1).EQ.ie-is+1 .AND. size(data,2).EQ.je-js+1 )then
          data_has_halos = .FALSE.
      else if( size(data,1).EQ.ied-isd+1 .AND. size(data,2).EQ.jed-jsd+1 )then
          data_has_halos = .TRUE.
      else
          call mpp_error( FATAL, 'MPP_WRITE: data must be either on compute domain or data domain.' )
      end if
      halos_are_global = x_is_global .AND. y_is_global
!      write(0,*)'MPP_WRITE_2DDECOMP_4D','checked for halos ',halos_are_global,npes
      if( npes.GT.1 .AND. mpp_file(unit)%threading.EQ.MPP_SINGLE )then
          if( halos_are_global )then
              call mpp_update_domains( data, domain )
!all non-0 PEs have passed their data to PE 0 and may now exit
!rv,sgi              if( pe.NE.0 )return
              if( pe.NE.mpp_root_pe() )return
              if(PRESENT(blockid)) then
                call write_record_b( unit, field, size(data), data, tstamp,block_id=blockid )
              else
                call write_record( unit, field, size(data), data, tstamp )
              endif
          else
!put field onto global domain
!              write(0,*)'MPP_WRITE_2DDECOMP_4D','allocating gdata'
              allocate( gdata(isg:ieg,jsg:jeg,size(data,3),size(data,4)) )
!              write(0,*)'MPP_WRITE_2DDECOMP_4D','calling mpp_global_field'
              call mpp_global_field( domain, data, gdata )
!              write(0,*)'MPP_WRITE_2DDECOMP_4D','called mpp_global_field'
!all non-0 PEs have passed their data to PE 0 and may now exit
!rv,sgi              if( pe.NE.0 )return
              if( pe.NE.mpp_root_pe())return
              if(PRESENT(blockid)) then
                call write_record_b( unit, field, size(gdata), gdata, tstamp ,block_id=blockid)
              else
                call write_record( unit, field, size(gdata), gdata, tstamp )
              endif
          end if
      else if( data_has_halos )then
!          write(0,*)'MPP_WRITE_2DDECOMP_4D','data_has_halos '
!store compute domain as contiguous data and pass to write_record
          allocate( cdata(is:ie,js:je,size(data,3),size(data,4)) )
          cdata(:,:,:,:) = data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:,:)
          if(PRESENT(blockid)) then
            call write_record_b( unit, field, size(cdata), cdata, tstamp, domain,block_id=blockid )
          else
            call write_record( unit, field, size(cdata), cdata, tstamp, domain )
          endif
!          write(0,*)'MPP_WRITE_2DDECOMP_4D','done with data_has_halos '
      else
!          write(0,*)'MPP_WRITE_2DDECOMP_4D','contigous ',size(data)
!data is already contiguous
          if(PRESENT(blockid)) then
            call write_record_b( unit, field, size(data), data, tstamp, domain,block_id=blockid )
          else
            call write_record( unit, field, size(data), data, tstamp, domain )
          endif
!          write(0,*)'MPP_WRITE_2DDECOMP_4D','done with contigous '
      end if
      return
    end subroutine MPP_WRITE_2DDECOMP_4D_
#undef use_local_CRI_pointers
