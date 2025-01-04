program day_11
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, i
    integer(int64) :: isum_1, isum_2
    integer(int64), dimension(:), allocatable :: stone_array
    integer(int64) :: start_count, stop_count, rate
    integer, parameter :: max_memo = 1024, current_year = 2024, max_blinks = 75
    integer(int64), dimension(0:max_memo, max_blinks) :: memo
    character(len=512) :: msg, line
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input11.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    read(io, "(A)") line
    close(io)
    stone_array = string_to_integers(line, " ")
    isum_1 = 0
    isum_2 = 0
    do i = 1, size(stone_array)
        isum_1 = isum_1 + blink(stone_array(i), 25)
        isum_2 = isum_2 + blink(stone_array(i), 75)
    end do
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
    
    recursive integer(int64) function blink(old_value, blinks_remaining) result(new_value)
        integer(int64), intent(in) :: old_value
        integer, intent(in) :: blinks_remaining
        integer(int64) :: left, right, temp
        integer :: length
        
        if (blinks_remaining == 0) then 
            new_value = 1
            return
        end if
        
        if (old_value == 0) then
            new_value = blink(1_int64, blinks_remaining - 1)
        else
            length = floor(log10(old_value * 1.d0) + 1)
            if (mod(length, 2) == 0) then
                
                right = mod(old_value, 10 ** (length / 2))
                left = (old_value - right) / (10 ** (length / 2))
                
                if (left <= max_memo .and. blinks_remaining > 1) then
                    if (memo(left, blinks_remaining - 1) == 0) then
                        memo(left, blinks_remaining - 1) = blink(left, blinks_remaining - 1)
                    end if
                    new_value = memo(left, blinks_remaining - 1)
                else
                    new_value = blink(left, blinks_remaining - 1)
                end if
                
                if (right <= max_memo .and. blinks_remaining > 1) then
                    if (memo(right, blinks_remaining - 1) == 0) then
                        memo(right, blinks_remaining - 1) = blink(right, blinks_remaining - 1)
                    end if
                    temp = memo(right, blinks_remaining - 1)
                else
                    temp = blink(right, blinks_remaining - 1)
                end if
                
                new_value = new_value + temp
            else
                new_value = blink(old_value * current_year, blinks_remaining - 1)
            end if
        end if
    end function blink
    
end program day_11