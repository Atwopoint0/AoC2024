program day_4
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, count
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    character(len=1), dimension(:,:), allocatable :: xmas_array
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input04.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    ! parse
    n_cols = number_of_cols_in_file(io)
    n_lines = number_of_lines_in_file(io)
    allocate(xmas_array(n_lines, n_cols))
    do i = 1, n_lines
        read(io, "(A)") line
        do j = 1, n_cols
            xmas_array(i,j) = line(j:j)
        end do
    end do
    close(io)
    
    ! part 1
    count = 0
    call check_rows(xmas_array, count, "XMAS")
    call check_cols(xmas_array, count, "XMAS")
    call check_diag(xmas_array, count, "XMAS")
    write(*,*) "Part 1: ", count
    
    ! part 2
    count = 0
    call check_cross(xmas_array, count, "MAS")
    write(*,*) "Part 2: ", count
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function number_of_cols_in_file(iunit) result(n_cols)
        integer, intent(in) :: iunit
        integer :: n_cols
        integer :: istat
        character(len=512) :: line
        rewind(iunit)
        n_cols = 0
        read(io, "(A)") line
        n_cols = len(trim(line))
        rewind(iunit)
    end function number_of_cols_in_file
    
    function number_of_lines_in_file(iunit) result(n_lines)
        integer, intent(in) :: iunit
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
    
    elemental function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n

        n = len(string)
        do i = 1, n
            reverse_string(n-i+1:n-i+1) = string(i:i)
        end do
    end function reverse
    
    subroutine check_rows(arr, count, needle)
        character(len=1), dimension(:,:), intent(in) :: arr
        character(len=*), intent(in) :: needle
        character(len=len(needle)) :: test_needle
        integer, intent(out) :: count
        integer :: i, j, k
        
        do i = 1, size(arr, dim=1)
            do j = 1, size(arr, dim=2) - len(needle) + 1
                test_needle = arr(i, j)
                do k = 1, len(needle) - 1
                    test_needle = trim(test_needle) // arr(i, j+k)
                end do
                if (test_needle == needle .or. test_needle == reverse(needle)) then
                    count = count + 1
                end if
            end do
        end do
    end subroutine check_rows
    
    subroutine check_cols(arr, count, needle)
        character(len=1), dimension(:,:), intent(in) :: arr
        character(len=*), intent(in) :: needle
        integer, intent(out) :: count
        call check_rows(transpose(arr), count, needle)
    end subroutine check_cols
    
    subroutine check_diag(arr, count, needle)
        character(len=1), dimension(:,:), intent(in) :: arr
        character(len=4), intent(in) :: needle
        integer, intent(out) :: count
        integer :: i, j, k
        character(len=len(needle)) :: test_needle

        !  positive diagonal

        do i = 1, size(arr, dim = 1) - len(needle) + 1
            do j = 1, size(arr, dim = 2) - len(needle) + 1
                test_needle = arr(i, j)
                do k = 1, len(needle) - 1
                    test_needle = trim(test_needle) // arr(i+k, j+k)
                end do
                if (test_needle == needle .or. test_needle == reverse(needle)) then
                    count = count + 1
                end if
            end do
        end do

        ! negative diagonal

        do i = 1, size(arr, dim = 1) - len(needle) + 1
            do j = size(arr, dim = 2), len(needle), -1
                test_needle = arr(i, j)
                do k = 1, len(needle) - 1
                    test_needle = trim(test_needle) // arr(i+k, j-k)
                end do
                if (test_needle == needle .or. test_needle == reverse(needle)) then
                    count = count + 1
                end if
            end do
        end do
    end subroutine check_diag
    
    subroutine check_cross(arr, count, needle)
        character(len=1), dimension(:,:), intent(in) :: arr
        character(len=*), intent(in) :: needle
        integer, intent(out) :: count
        integer :: i, j, k
        character(len=len(needle)) :: left_test, right_test
        logical :: left_match, right_match

        do i = 1, size(arr, dim = 1) - len(needle) + 1 
            do j = 1, size(arr, dim = 2) - len(needle) + 1
                left_match = .FALSE. 
                right_match = .FALSE. 

                left_test = arr(i, j)
                right_test = arr(i, j+len(needle)-1)
                do k = 1, len(needle) - 1
                    left_test = trim(left_test) // arr(i+k, j+k)
                    right_test = trim(right_test) // arr(i+k, j+len(needle)-1-k)
                end do    

                left_match = left_test == needle .or. left_test == reverse(needle)
                right_match = right_test == needle .or. right_test == reverse(needle)
                
                if (left_match .and. right_match) then
                    count = count + 1
                end if
            end do
        end do
    end subroutine check_cross
    
end program day_4