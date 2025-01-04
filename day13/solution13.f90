program day_13
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, comma_index
    integer(int64), parameter :: unit_conversion = 10000000000000
    integer, dimension(2), parameter :: token_cost = [3, 1]
    integer(int64) :: isum_1, isum_2
    integer(int64) :: start_count, stop_count, rate
    integer(int64), dimension(2, 2) :: ab
    integer(int64), dimension(2) :: xy_1, xy_2
    character(len=512) :: msg, line
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input13.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    isum_1 = 0
    isum_2 = 0
    do
        read(io, "(A)") line
        read(line(13:), *) ab(1, 1)
        read(line(19:), *) ab(2, 1)

        read(io, "(A)") line
        read(line(13:), *) ab(1, 2)
        read(line(19:), *) ab(2, 2)
        
        read(io, "(A)") line
        comma_index = index(line, ",")
        read(line(10:), *) xy_1(1)
        read(line(comma_index + 4:), *) xy_1(2)
        
        xy_2 = unit_conversion + xy_1
        isum_1 = isum_1 + sum(cramer(ab, xy_1) * token_cost)
        isum_2 = isum_2 + sum(cramer(ab, xy_2) * token_cost)
        read(io, *, iostat = stat)
        if (stat /= 0) exit
    end do
    close(io)
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2

    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function cramer(mat, vec) result(sol)
        integer(int64), dimension(2,2), intent(in) :: mat
        integer(int64), dimension(2), intent(in) :: vec
        integer(int64), dimension(2) :: sol
        integer(int64), dimension(2) :: r
        integer(int64) :: det
        ! |a b| . |u| = |x|
        ! |c d|   |v|   |y|
        det = mat(1, 1) * mat(2, 2) - mat(1, 2) * mat(2, 1) ! ad - bc
        if (det == 0) error stop "Singular matrix!"
        r(1) = mat(2, 2) * vec(1) - mat(1, 2) * vec(2) ! first minor
        r(2) = - mat(2, 1) * vec(1) + mat(1, 1) * vec(2) ! second minor
        if (all(mod(r, det) == 0) .and. all(r / det >= 0)) then ! find positive integer solutions
            sol = r / det
        else
            sol = 0
        end if
   end function cramer
    
end program day_13