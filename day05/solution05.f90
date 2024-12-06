program day_5
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, n, isum_1, isum_2, mid
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    integer, dimension(:,:), allocatable :: rules
    integer, dimension(:), allocatable :: update
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input05.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n = 0
    do 
        read(io, "(A)", iostat = stat) line
        if (line == " ") exit
        n = n + 1
    end do
    allocate(rules(2, n))
    rewind(io)
    do i = 1, n
        read(io, "(A)") line
        line = replace(line, ["|"], ",")
        read(line, *) rules(:, i)
    end do
    read(io, *) ! skip blank line
    
    isum_1 = 0
    isum_2 = 0
    do
        read(io, "(A)", iostat=stat) line
        if (stat /= 0) exit
        update = string_to_integers(line, ",")
        mid = size(update) / 2 + 1
        if (order_check(update, rules)) then
            ! part 1
            isum_1 = isum_1 + update(mid)
        else
            ! part 2
            ! basic implementation of bubble sort
            do i = 1, size(update) - 1
                do j = 1, size(update) - i
                    if (.not. order_check(update(j:j+1), rules)) then
                        update(j:j+1) = update(j+1:j : -1) ! swap
                    end if
                end do
            end do
            isum_2 = isum_2 + update(mid)
        end if
        deallocate(update)
    end do
    deallocate(rules)
    close(io)
    
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    function string_to_integers(str, sep) result(a)
        integer, allocatable :: a(:)
        character(*) :: str
        character :: sep
        integer :: i, n_sep

        n_sep = 0
        do i = 1, len_trim(str)
            if (str(i:i) == sep) then
                n_sep = n_sep + 1
                str(i:i) = ','
            end if
        end do
        allocate(a(n_sep + 1))
        read(str,*) a
    end function
    
    function replace(string, charset, target_char) result(res)
        character(*), intent(in) :: string
        character, intent(in) :: charset(:), target_char
        character(len(string)) :: res
        integer :: n
        res = string
        do n = 1, len(string)
            if (any(string(n:n) == charset)) then
                res(n:n) = target_char
            end if
        end do
    end function replace
    
    logical function order_check(a, cond) result(res)
        integer, intent(in) :: a(:)
        integer, intent(in) :: cond(:,:)
        integer :: i, r1, r2
        integer :: idx(maxval(cond))
        idx = 0
        do i = 1, size(a)
            idx(a(i)) = i
        end do
        res = .true.
        do i = 1, size(rules, dim = 2)
            r1 = idx(cond(1, i))
            r2 = idx(cond(2, i))
            if (r2 /= 0 .and. r2 <= r1) then
                res = .false.
                return
            end if
        end do
    end function order_check
end program day_5