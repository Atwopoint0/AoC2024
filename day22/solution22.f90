program day22
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, j, k
    integer :: a, b, prev_leading_digit, next_leading_digit, sequence_id
    integer(int64) :: num, isum_1, isum_2
    integer, dimension(4) :: sequence
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(:,:), allocatable :: price
    character(len=512) :: line, msg
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input22.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    ! Create a giant table with all possible sequences (19*19*19*19 possibilities)
    allocate(price(19**4, n_lines), source = -1)
    isum_1 = 0
    do i = 1, n_lines
        read(io, *) num
        sequence = [0, 0, 0, 0]
        prev_leading_digit = modulo(num, 10)
        do j = 1, 2000
            num = next_num(num)
            next_leading_digit = modulo(num, 10)
            do k = 1, 3
                sequence(k) = sequence(k + 1)
            end do
            sequence(4) = next_leading_digit - prev_leading_digit
            prev_leading_digit = next_leading_digit
            if (j <= 3) cycle
            sequence_id = seq_hash(sequence)
            if (price(sequence_id, i) < 0) then
                price(sequence_id, i) = next_leading_digit
            end if
        end do
        isum_1 = isum_1 + num
    end do
    close(io)
    
    write(*,*) "Part 1: ", isum_1
    
    do sequence_id = 1, 19 ** 4
        b = sequence_id - 1
        ! Undo the hashmap
        do i = 4, 1, -1
            sequence(i) = modulo(b, 19) - 9
            b = b / 19
        end do
        a = sum(price(sequence_id, :), (price(sequence_id, :) >= 0))
        if (a == 0) cycle
        if (a > isum_2) isum_2 = a
    end do

    write(*,*) "Part 2: ", isum_2

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
    
    integer(int64) function next_num(x) result(res)
        integer(int64), value :: x
        x = modulo(ieor(x, x * 64_int64), 16777216_int64)
        x = modulo(ieor(x, x / 32_int64), 16777216_int64)
        res = modulo(ieor(x, x * 2048_int64), 16777216_int64)
    end function next_num
    
    integer function seq_hash(seq) result(id)
        integer, dimension(4), intent(in) :: seq
        integer, dimension(4) :: tmp
        integer :: i 
        tmp = seq + 9
        id = tmp(1)
        do i = 2, 4
            id = id * 19 + tmp(i)
        end do
        id = id + 1
    end function seq_hash
    
end program day22