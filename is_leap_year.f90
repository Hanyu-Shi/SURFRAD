logical function is_leap_year(year)
    implicit none
    integer::year
    is_leap_year = .false.
    if((mod(year,400)==0) .or. ((mod(year,4)==0) .and. (mod(year,100) /= 0))) then
        is_leap_year = .true.
    endif
    return
end function is_leap_year
