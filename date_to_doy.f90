integer function date_to_doy(year,month,day)
    implicit none
    integer::year,month,day,k,doy
    integer::days_of_months(12)
    logical,external::is_leap_year

    days_of_months = (/0,31,28,31,30,31,30,31,31,30,31,30/)
    if(is_leap_year(year)) days_of_months(3) = 29

    doy = 0
    do k=1, month
        doy = doy + days_of_months(k)
    enddo
    date_to_doy = doy + day
    return
end function date_to_doy
