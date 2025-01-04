program day_19
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, max_len
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    character(len=3000) :: str
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input19.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)    
    read(io, "(A)") str
    
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function number_of_lines_in_file(iunit) result(n_lines)
        integer, intent(in) :: iunit
        integer :: n_lines
        character(len=1) :: tmp
        integer :: istat
        rewind(iunit)
        n_lines = 0
        do
            read(iunit, fmt = '(A1)', iostat = istat) tmp
            if (is_iostat_end(istat)) exit
            n_lines = n_lines + 1
        end do
        rewind(iunit)
    end function number_of_lines_in_file
    
end program day_19