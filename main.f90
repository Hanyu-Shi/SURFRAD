program main
    implicit none
    character(len=2)::tmp1,tmp3,tmp4
    character(len=3)::site_name,tmp2
    character(len=4)::tmp5
    character(len=80)::infolder,outfolder,datafolder
    character(len=100)::infile,outfile_albedo,outfile_PAR,outfile_aod
    integer::year,i,nlines,j,doy
    logical::alive
    integer::month_days_arr(12)
    logical,external::is_leap_year
    integer,external::date_to_doy

    datafolder = "Bondville"
    site_name = "bon"
    year = 2005

    ! deal rad files
    infolder = "G:/"//trim(adjustl(datafolder))//"/surfrad/rad/"
    outfolder = "G:/"//trim(adjustl(datafolder))//"/surfrad/"
    outfile_albedo = trim(adjustl(trim(outfolder) // "surfrad_albedo"))
    outfile_PAR = trim(adjustl(trim(outfolder) // "surfrad_PAR"))

    call clear_file_content(outfile_albedo)
    call clear_file_content(outfile_PAR)

    write(tmp1,'(I2.2)') mod(year,100)
    do i=1,366
        write(tmp2,'(I3.3)') i
        infile = trim(adjustl(trim(infolder) // site_name // tmp1 // tmp2 // ".dat"))

        inquire(file=infile,exist=alive)
        if(.not. alive) cycle
        call file_lines(infile,nlines)
        call read_rad_data(infile,outfile_albedo,outfile_PAR,nlines-2)
    enddo

    ! deal aod files
    infolder = "D:/Aerosol/"//trim(adjustl(datafolder))//"/surfrad/aod/"
    outfolder = "D:/Aerosol/"//trim(adjustl(datafolder))//"/surfrad/"
    outfile_aod = trim(adjustl(trim(outfolder) // "surfrad_aod"))

    month_days_arr = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    if (is_leap_year(year)) month_days_arr(2) = 29
    write(tmp5,'(I4.4)') year
    call clear_file_content(outfile_aod)
    do i=1,12
        write(tmp3,'(I2.2)') i
        do j=1,month_days_arr(i)
            write(tmp4,'(I2.2)') j
            infile = trim(adjustl(trim(infolder) //site_name//"_"//tmp5//tmp3//tmp4//".aod"))
            inquire(file=infile,exist=alive)
            if(.not. alive) cycle
            call file_lines(infile,nlines)
            doy = date_to_doy(year,i,j)
            call read_aod_data(infile,outfile_aod,nlines-6,doy)
        enddo
    enddo

end program main

subroutine read_aod_data(infile,outfile_aod,nlines,doy)
    implicit none
    character(len=100)::headline,infile,outfile_aod
    integer::nlines,ltime(nlines),qc(nlines)
    real::aod502(nlines),aod614(nlines),tmp
    integer::i,lun_in,lun_out,doy
    real::time,angstrom,aod550

    lun_in = 17
    lun_out = 18
    open(unit=lun_in,file=trim(infile),status='old')
    do i=1,6
        read(lun_in,*)headline
    enddo
    do i=1,nlines
        read(lun_in,'(I4,5x,I1,3x,5(1x,f6.3),5(1x,f7.4),1x,f6.1,1x,f6.3)') &
            ltime(i),qc(i),tmp,aod502(i),aod614(i),tmp,tmp,tmp,tmp,tmp,tmp,tmp,&
            tmp,tmp
    enddo

    open(unit=lun_out,file=trim(outfile_aod),status='old',position='append')
    do i=1,nlines
        if(qc(i)==0) then
            time = doy + (ltime(i)/100 + mod(ltime(i),100)/60.0)/24.0
            ! HyS, add angstrom and aod550
            call cal_ang_aod550(aod502(i),aod614(i),angstrom,aod550)
            !write(lun_out,'(f8.4,2x,f6.3,2x,f6.3)')time,aod502(i),aod614(i)
            write(lun_out,'(f8.4,2x,f6.3,2x,f6.3,2f10.6)')time,aod502(i),aod614(i),angstrom,aod550
        endif
    enddo
end subroutine read_aod_data

subroutine cal_ang_aod550(aod502,aod614,angstrom,aod550)
    implicit none
    real, intent(in) :: aod502,aod614
    real, intent(out) :: aod550,angstrom

    angstrom = log(aod502/aod614) / log(613.5/501.6)
    aod550 = aod502*(501.5/550.0)**angstrom

    return
end subroutine


subroutine read_rad_data(infile,outfile_albedo,outfile_PAR,nlines)
    implicit none
    integer::nlines
    character(len=100)::station_name, infile, outfile_albedo,outfile_PAR

    integer::year,month,day,jday,elevation
    integer::minute(nlines),hour(nlines)

    real::latitude,longitude,dt(nlines),zen(nlines),direct_n(nlines)
    real::dw_solar(nlines),uw_solar(nlines)
    real::diffuse(nlines),dw_ir(nlines),dw_casetemp(nlines)
    real::dw_dometemp(nlines),uw_ir(nlines),uw_casetemp(nlines)
    real::uw_dometemp(nlines),uvb(nlines),par(nlines)
    real::netsolar(nlines),netir(nlines),totalnet(nlines),temp(nlines)
    real::rh(nlines),windspd(nlines),winddir(nlines),pressure(nlines)

    integer::qc_direct_n(nlines),qc_netsolar(nlines),qc_netir(nlines)
    integer::qc_dwsolar(nlines),qc_uwsolar(nlines),qc_diffuse(nlines)
    integer::qc_dwir(nlines),qc_dwcasetemp(nlines)
    integer::qc_dwdometemp(nlines),qc_uwir(nlines)
    integer::qc_uwcasetemp(nlines),qc_uwdometemp(nlines)
    integer::qc_uvb(nlines),qc_par(nlines)
    integer::qc_totalnet(nlines),qc_temp(nlines)
    integer::qc_rh(nlines),qc_windspd(nlines),qc_winddir(nlines)
    integer::qc_pressure(nlines)

    integer::icount,i,lun_in,lun_out
    real::time,albedo

    lun_in = 20
    open(unit=lun_in,file=trim(infile),status='old')
    read(lun_in,'(1x,A100)') station_name
    read(lun_in,*)latitude, longitude, elevation

    icount = 0
    do i = 1,nlines
        read(lun_in,'(1x,i4,1x,i3,4(1x,i2),1x,f6.3,1x,f6.2,20(1x,f7.1,1x,i1))') &
            year,jday,month,day,hour(i),minute(i),dt(i),zen(i), &
            dw_solar(i),qc_dwsolar(i),uw_solar(i),qc_uwsolar(i),direct_n(i), &
            qc_direct_n(i),diffuse(i),qc_diffuse(i),dw_ir(i),qc_dwir(i), &
            dw_casetemp(i),qc_dwcasetemp(i),dw_dometemp(i),qc_dwdometemp(i), &
            uw_ir(i),qc_uwir(i),uw_casetemp(i),qc_uwcasetemp(i),uw_dometemp(i), &
            qc_uwdometemp(i),uvb(i),qc_uvb(i),par(i),qc_par(i),netsolar(i), &
            qc_netsolar(i),netir(i),qc_netir(i),totalnet(i),qc_totalnet(i), &
            temp(i),qc_temp(i),rh(i),qc_rh(i),windspd(i),qc_windspd(i),winddir(i), &
            qc_winddir(i),pressure(i),qc_pressure(i)

        icount = icount + 1
    enddo
    close(lun_in)

    lun_out = 20
    open(unit=lun_out,file=trim(outfile_albedo),status='old',position='append')
    do i = 1, icount
        if((qc_dwsolar(i)+qc_uwsolar(i)) == 0 .and. (uw_solar(i)>=0)  &
            .and. (dw_solar(i)>uw_solar(i))) then
            time = jday + dt(i) / 24.0
            albedo = uw_solar(i) / dw_solar(i)
            write(lun_out,"(f8.4,2x,f6.4)")time,albedo
        endif
    enddo
    close(lun_out)

    lun_out = 20
    open(unit=lun_out,file=trim(outfile_PAR),status='old',position='append')
    do i = 1, icount
        if(qc_par(i)==0) then
            time = jday + dt(i) / 24.0
            write(lun_out,"(f8.4,2x,f7.1)")time,par(i)
        endif
    enddo
    close(lun_out)

end subroutine read_rad_data
