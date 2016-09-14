subroutine file_lines(filename,nlines)
    implicit none
    character(len=100)::filename,tmp
    integer::nlines,file_stat,file_unit

    file_unit = 11
    nlines = 0
    file_stat = 0

    open(file_unit,file=trim(adjustl(filename)),status='old')
    do while(file_stat==0)
        read(file_unit,*,iostat=file_stat)tmp  ! may be need to modified to avoid the last empty lines
        nlines = nlines + 1
    enddo
    nlines = nlines - 1
    close(file_unit)
end subroutine file_lines
