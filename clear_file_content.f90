subroutine clear_file_content(filename)
    implicit none
    integer::lun
    character(len=100)::filename
    lun = 13
    open(unit=lun,file=trim(adjustl(filename)),status='replace')
    close(lun)
end subroutine clear_file_content
