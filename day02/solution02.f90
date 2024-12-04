program day_2
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, j, n, isum1, isum2
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    integer, dimension(:), allocatable :: nums
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input02.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    
    isum1 = 0
    isum2 = 0
    do i = 1, n_lines
        read(io, "(A)",  iostat = stat) line
        nums = string_to_integers(line, " ")
        if (safe(nums)) then
            isum1 = isum1 + 1
            isum2 = isum2 + 1
        else
            do j = 1, size(nums)
                if (safe([nums(1:j-1), nums(j+1:size(nums))])) then
                    isum2 = isum2 + 1
                    exit
                end if
            end do
        end if
    end do
    close(io)
    
    write(*,*) "Part 1: ", isum1
    write(*,*) "Part 2: ", isum2
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function number_of_lines_in_file(iunit) result(n_lines)
        integer,intent(in)  :: iunit
        integer :: n_lines
        character(len=1) :: tmp
        integer :: istat
        rewind(iunit)
        n_lines = 0
        do
            read(iunit,fmt='(A1)',iostat=istat) tmp
            if (is_iostat_end(istat)) exit
            n_lines = n_lines + 1
        end do
        rewind(iunit)
    end function number_of_lines_in_file
    
    function string_to_integers(str, sep) result(a)
        integer, allocatable :: a(:)
        character(*) :: str
        character :: sep
        integer :: i, n_sep

        n_sep = 0
        do i = 1, len_trim(str)
            if (str(i:i)==sep) then
                n_sep = n_sep + 1
                str(i:i) = ','
            end if
        end do
        allocate(a(n_sep+1))
        read(str,*) a
    end function
    
    logical function safe(ints)
        integer,dimension(:),intent(in) :: ints
        integer :: j, difference
        logical :: increasing, decreasing, diffcheck

        increasing = .true.
        decreasing = .true.
        diffcheck = .true.
        
        do j = 1, size(ints)-1
            if (ints(j+1) > ints(j)) decreasing = .false.
            if (ints(j+1) < ints(j)) increasing = .false.
            difference = abs(ints(j + 1) - ints(j))
            if (difference == 0 .or. difference > 3) diffcheck = .false.
            safe = ((increasing .or. decreasing) .and. diffcheck)
            if (.not. safe) exit
        end do
    end function safe
    
end program day_2