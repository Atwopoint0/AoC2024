program day_7
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, j
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    integer(int64), dimension(:), allocatable :: instruction
    integer(int64) :: answer, isum_1, isum_2
    logical :: flag
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input07.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    isum_1 = 0
    isum_2 = 0
    do i = 1, n_lines
        read(io, "(A)") line
        line = replace(line, ":", " ")
        instruction = string_to_integers(line, " ")
        answer = instruction(1)
        flag = .false.
        ! part 1
        call eval(instruction(4:), instruction(3), answer, flag, allow_concatenate = .false.)
        if (flag) then
            isum_1 = isum_1 + answer
        else
            ! part 2
            call eval(instruction(4:), instruction(3), answer, flag, allow_concatenate = .true.)
            if (flag) then 
                isum_2 = isum_2 + answer
            end if
        end if
        deallocate(instruction)
    end do
    close(io)
    
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2 + isum_1
    
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
            read(iunit,fmt='(A1)',iostat=istat) tmp
            if (is_iostat_end(istat)) exit
            n_lines = n_lines + 1
        end do
        rewind(iunit)
    end function number_of_lines_in_file
    
    pure function replace(string, charset, target_char) result(res)
        character(*), intent(in) :: string
        character, intent(in) :: charset, target_char
        character(len(string)) :: res
        integer :: n
        res = string
        do n = 1, len(string)
            if (string(n:n) == charset) then
                res(n:n) = target_char
            end if
        end do
    end function replace
    
    function string_to_integers(str, sep) result(a)
        integer(int64), allocatable :: a(:)
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
    
    recursive subroutine eval(elements, num, answer, flag, allow_concatenate)
        integer(int64), dimension(:), intent(inout) :: elements
        integer(int64), value, intent(in) :: num
        integer(int64), intent(in) :: answer
        logical, intent(out) :: flag
        logical, intent(in) :: allow_concatenate
        if (flag) return
        if (size(elements) == 0) then
            flag = (num == answer)
            return
        end if
        if (num * elements(1) <= answer) then
            call eval(elements(2:), num * elements(1), answer, flag, allow_concatenate)
        end if
        if (num + elements(1) <= answer) then
            call eval(elements(2:), num + elements(1), answer, flag, allow_concatenate)
        end if 
        if (allow_concatenate .and. combine(num, elements(1)) <= answer) then
            call eval(elements(2:), combine(num, elements(1)), answer, flag, allow_concatenate)
        end if
    end subroutine
    
    integer(int64) function combine(a,b) result(res)
        integer(int64), value, intent(in) :: a, b
        res = a * 10 ** int(log10(b * 1.d0) + 1) + b 
    end function combine
    
end program day_7