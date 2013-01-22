#ifdef use_CRI_pointers 
#undef use_local_CRI_pointers
#endif
    subroutine MPP_READ_2DDECOMP_1D_(unit,field,domain,data,tindex,blockid )
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(in) :: domain
      MPP_TYPE_, intent(inout) :: data(:)
      integer, intent(in), optional :: tindex
      integer, intent(in), optional :: blockid
      integer::il_xbegin,il_xend,il_ybegin,il_yend
      MPP_TYPE_,allocatable :: data3D(:,:,:)
        
      call mpp_get_compute_domain(domain &
                                ,xbegin=il_xbegin,xend=il_xend &
                                ,ybegin=il_ybegin,yend=il_yend)

      allocate(data3D(1:il_xend-il_xbegin+1,1:il_yend-il_ybegin+1,1:1))
      data3D = RESHAPE( data, SHAPE(data3D) )
      if(PRESENT(blockid)) then
        call mpp_read( unit,field,domain,data3D,tindex,blockid)
      else
        call mpp_read( unit, field, domain, data3D, tindex )
      endif
      data   = RESHAPE( data3D, SHAPE(data) )
      deallocate(data3D)
          
      return
    end subroutine MPP_READ_2DDECOMP_1D_
    subroutine MPP_READ_2DDECOMP_2D_( unit, field, domain,data,tindex,blockid )
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(in) :: domain
      MPP_TYPE_, intent(inout) :: data(:,:)
      integer, intent(in), optional :: tindex
      integer, intent(in), optional :: blockid
      MPP_TYPE_ :: data3D(size(data,1),size(data,2),1)
#ifdef use_local_CRI_pointers
      pointer( ptr, data3D )
      ptr = LOC(data)
      if(PRESENT(blockid)) then
      call mpp_read( unit, field, domain, data3D, tindex, blockid )
      else
      call mpp_read( unit, field, domain, data3D, tindex )
      endif
#else
      data3D = RESHAPE( data, SHAPE(data3D) )
      if(PRESENT(blockid)) then
      call mpp_read( unit, field, domain, data3D, tindex, blockid )
      else
      call mpp_read( unit, field, domain, data3D, tindex )
      endif
      data   = RESHAPE( data3D, SHAPE(data) )
#endif
      return
    end subroutine MPP_READ_2DDECOMP_2D_

    subroutine MPP_READ_2DDECOMP_3D_(unit,field,domain,data,tindex,blockid )
!mpp_read reads <data> which has the domain decomposition <domain>
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(in) :: domain
      MPP_TYPE_, intent(inout) :: data(:,:,:)
      integer, intent(in), optional :: tindex
      integer, intent(in), optional :: blockid
      MPP_TYPE_, allocatable :: cdata(:,:,:)
      MPP_TYPE_, allocatable :: gdata(:)
      integer :: len, lenx,leny,lenz,i,j,k,n
!NEW: data may be on compute OR data domain
      logical :: data_has_halos, halos_are_global, x_is_global, y_is_global
      integer :: is, ie, js, je, isd, ied, jsd, jed, isg, ieg, jsg, jeg, ioff, joff

      if (.NOT. present(tindex) .AND. mpp_file(unit)%time_level .ne. -1) &
      call mpp_error(FATAL, 'MPP_READ: need to specify a time level for data with time axis')

      if( .NOT.module_is_initialized )call mpp_error( FATAL, 'MPP_READ: must first call mpp_io_init.' )
      if( .NOT.mpp_file(unit)%opened )call mpp_error( FATAL, 'MPP_READ: invalid unit number.' )

      call mpp_get_compute_domain( domain, is,  ie,  js,  je  )
      call mpp_get_data_domain   ( domain, isd, ied, jsd, jed, x_is_global=x_is_global, y_is_global=y_is_global )
      call mpp_get_global_domain ( domain, isg, ieg, jsg, jeg )
      if( debug ) &
      write(0,*) 'MPP_READ_2DDECOMP_3D_:is,  ie,  js,  je, isd, ied, jsd, jed',is,  ie,  js,  je, isd, ied, jsd, jed
      if( debug ) &
      write(0,*) 'MPP_READ_2DDECOMP_3D_:isg, ieg, jsg, jeg',isg, ieg, jsg, jeg
      if( size(data,1).EQ.ie-is+1 .AND. size(data,2).EQ.je-js+1 )then
          data_has_halos = .FALSE.
      else if( size(data,1).EQ.ied-isd+1 .AND. size(data,2).EQ.jed-jsd+1 )then
          data_has_halos = .TRUE.
      else
          call mpp_error( FATAL, 'MPP_READ: data must be either on compute domain or data domain.' )
      end if
      halos_are_global = x_is_global .AND. y_is_global
      if( debug ) &
      write(0,*) 'MPP_READ_2DDECOMP_3D_: halos_are_global', halos_are_global
      if( npes.GT.1 .AND. mpp_file(unit)%threading.EQ.MPP_SINGLE )then
      if( debug ) &
      write(0,*) 'MPP_READ_2DDECOMP_3D_: threading.EQ.MPP_SINGLE'
          if( halos_are_global )then !you can read directly into data array
              if(PRESENT(blockid)) then
              if( pe.EQ.mpp_root_pe() )call read_record_b(unit,field,size(data) &
                                           ,data,tindex,block_id=blockid )
              else
              if(pe.EQ.mpp_root_pe())call read_record(unit,field,size(data),data,tindex )
              endif
          else
              lenx=ieg-isg+1
              leny=jeg-jsg+1
              lenz=size(data,3)
              len=lenx*leny*lenz
              write(0,*)'MPP_READ_2DDECOMP_3D: gdata ', lenx,leny,lenz
              allocate(gdata(len))          
! read field on pe 0 and pass to all pes
              if(PRESENT(blockid)) then
              if(pe.EQ.mpp_root_pe()) call read_record_b(unit,field,len,gdata,tindex,block_id=blockid)
              else
              if( pe.EQ.mpp_root_pe() ) call read_record( unit, field, len, gdata, tindex )
              endif
! broadcasting global array, this can be expensive!          
              call mpp_transmit( gdata, len, ALL_PES, gdata, len, 0 )
              ioff = is; joff = js
              if( data_has_halos )then
                  ioff = isd; joff = jsd
              end if
              do k=1,size(data,3)
                 do j=js,je
                    do i=is,ie
                       n=(i-isg+1) + (j-jsg)*lenx + (k-1)*lenx*leny
                       data(i-ioff+1,j-joff+1,k)=gdata(n)
                    enddo
                 enddo
              enddo
              deallocate(gdata)
          end if
      else if( data_has_halos )then
! for uniprocessor or multithreaded read
! read compute domain as contiguous data

          allocate( cdata(is:ie,js:je,size(data,3)) )
          if(PRESENT(blockid)) then
          call read_record_b(unit,field,size(cdata),cdata,tindex,domain,block_id=blockid)
          else
          call read_record(unit,field,size(cdata),cdata,tindex,domain)
          endif

          data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:) = cdata(:,:,:)
          deallocate(cdata)
      else
          if(PRESENT(blockid)) then
          call read_record_b(unit,field,size(data),data,tindex,domain,block_id=blockid)
          else
          call read_record(unit,field,size(data),data,tindex,domain)
          endif
      end if

      return
    end subroutine MPP_READ_2DDECOMP_3D_

    subroutine MPP_READ_2DDECOMP_4D_(unit,field,domain,data,tindex,blockid )
!mpp_read reads <data> which has the domain decomposition <domain>
      integer, intent(in) :: unit
      type(fieldtype), intent(in) :: field
      type(domain2D), intent(in) :: domain
      MPP_TYPE_, intent(inout) :: data(:,:,:,:)
      integer, intent(in), optional :: tindex
      integer, intent(in), optional :: blockid
      MPP_TYPE_, allocatable :: cdata(:,:,:,:)
      MPP_TYPE_, allocatable :: gdata(:)
      integer :: len, lenx,leny,lenz,i,j,k,n,l
!NEW: data may be on compute OR data domain
      logical :: data_has_halos, halos_are_global, x_is_global, y_is_global
      integer :: is, ie, js, je, isd, ied, jsd, jed, isg, ieg, jsg, jeg, ioff, joff

      if (.NOT. present(tindex) .AND. mpp_file(unit)%time_level .ne. -1) &
      call mpp_error(FATAL, 'MPP_READ: need to specify a time level for data with time axis')

      if( .NOT.module_is_initialized )call mpp_error( FATAL, 'MPP_READ: must first call mpp_io_init.' )
      if( .NOT.mpp_file(unit)%opened )call mpp_error( FATAL, 'MPP_READ: invalid unit number.' )

      call mpp_get_compute_domain( domain, is,  ie,  js,  je  )
      call mpp_get_data_domain   ( domain, isd, ied, jsd, jed, x_is_global=x_is_global, y_is_global=y_is_global )
      call mpp_get_global_domain ( domain, isg, ieg, jsg, jeg )
      if( size(data,1).EQ.ie-is+1 .AND. size(data,2).EQ.je-js+1 )then
          data_has_halos = .FALSE.
      else if( size(data,1).EQ.ied-isd+1 .AND. size(data,2).EQ.jed-jsd+1 )then
          data_has_halos = .TRUE.
      else
          call mpp_error( FATAL, 'MPP_READ: data must be either on compute domain or data domain.' )
      end if
      halos_are_global = x_is_global .AND. y_is_global
      if( npes.GT.1 .AND. mpp_file(unit)%threading.EQ.MPP_SINGLE )then
          if( halos_are_global )then !you can read directly into data array
              if(PRESENT(blockid)) then
              if( pe.EQ.mpp_root_pe() )call read_record_b(unit,field,size(data) &
                                           ,data,tindex,block_id=blockid )
              else
              if(pe.EQ.mpp_root_pe())call read_record(unit,field,size(data),data,tindex )
              endif
          else
              lenx=ieg-isg+1
              leny=jeg-jsg+1
              lenz=size(data,3)
              len=lenx*leny*lenz*size(data,4)
              allocate(gdata(len))          
! read field on pe 0 and pass to all pes
              if(PRESENT(blockid)) then
              if(pe.EQ.mpp_root_pe()) call read_record_b(unit,field,len,gdata,tindex,block_id=blockid)
              else
              if( pe.EQ.mpp_root_pe() ) call read_record( unit, field, len, gdata, tindex )
              endif
! broadcasting global array, this can be expensive!          
              call mpp_transmit( gdata, len, ALL_PES, gdata, len, 0 )
              ioff = is; joff = js
              if( data_has_halos )then
                  ioff = isd; joff = jsd
              end if
              do l=1,size(data,4)
                do k=1,size(data,3)
                  do j=js,je
                    do i=is,ie
                       n=(i-isg+1)+(j-jsg)*lenx+(k-1)*lenx*leny &
                        +(l-1)*lenx*leny*lenz
                       data(i-ioff+1,j-joff+1,k,l)=gdata(n)
                    enddo
                  enddo
                enddo
              enddo
              deallocate(gdata)
          end if
      else if( data_has_halos )then
! for uniprocessor or multithreaded read
! read compute domain as contiguous data

          allocate( cdata(is:ie,js:je,size(data,3),size(data,4)) )
          if(PRESENT(blockid)) then
          call read_record_b(unit,field,size(cdata),cdata,tindex,domain,block_id=blockid)
          else
          call read_record(unit,field,size(cdata),cdata,tindex,domain)
          endif

          data(is-isd+1:ie-isd+1,js-jsd+1:je-jsd+1,:,:) = cdata(:,:,:,:)
          deallocate(cdata)
      else
          if(PRESENT(blockid)) then
          call read_record_b(unit,field,size(data),data,tindex,domain,block_id=blockid)
          else
          call read_record(unit,field,size(cdata),cdata,tindex,domain)
          endif
      end if

      return
    end subroutine MPP_READ_2DDECOMP_4D_
#define use_local_CRI_pointers
