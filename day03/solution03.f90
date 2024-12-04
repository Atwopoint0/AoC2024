program day_3
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines
    integer :: read_state, mul_state, digit_state
    integer :: isum_1, isum_2, num_1, num_2
    integer(int64) :: start_count, stop_count, rate
    logical :: do_toggle
    character(len=1) :: char
    character(:), allocatable :: do_statement
    character(len=10) :: buffer
    character(len=512) :: msg
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input03.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg, access = "stream")
    
    isum_1 = 0
    isum_2 = 0
    read_state = 0
    digit_state = 0
    mul_state = 0
    buffer = ""
    do_toggle = .true.
    do_statement = ""
    
    do
        read(io, iostat = stat) char
        if (stat /= 0) exit
        
        select case(read_state)
            
        case(0) ! find instruction
            
            select case(mul_state)    
                
            case(0) ! find "m"
                if (char == "m") then
                    mul_state = 1
                else
                    mul_state = 0
                end if
                
            case(1) ! find "u"
                if (char == "u") then
                    mul_state = 2
                else
                    mul_state = 0
                end if
                
            case(2) ! find "l"
                if (char == "l") then
                    mul_state = 3
                else
                    mul_state = 0
                end if
                
            case(3) ! find "("
                if (char == "(") then
                    read_state = 1
                    mul_state = 0
                else
                    mul_state = 0
                end if
                
            end select
            
        case(1) ! find numbers
            
            select case(iachar(char)) ! integer ASCII
                
            case(48:57) ! digits in positions 48 to 57
                buffer = trim(buffer) // char
                
            case (44) ! "," in position 44
                if (digit_state == 0 .and. len(trim(buffer)) > 0) then
                    read(buffer, *) num_1
                    buffer = ""
                    digit_state = 1
                else
                    read_state = 0
                    digit_state = 0
                    buffer = ""
                end if
            
            case (41) ! ")" in position 41
                if (digit_state == 1 .and. len(trim(buffer)) > 0) then
                    read(buffer, *) num_2
                    read_state = 2
                    digit_state = 0
                    buffer = ""
                else
                    read_state = 0
                    digit_state = 0
                    buffer = ""
                end if
                
            case default ! invalid instruction
                read_state = 0
                digit_state = 0
                buffer = ""
                
            end select
            
        end select
        
        if (do_toggle == .true.) then
            if (len(do_statement) < 7) then
                do_statement = do_statement // char
            else
                do_statement = do_statement(2:) // char
            end if
            if (trim(do_statement) == "don't()") then
                do_toggle = .false.
                do_statement = ""
            end if
        else
            if (len(do_statement) < 4) then
                do_statement = do_statement // char
            else
                do_statement = do_statement(2:) // char
            end if
            if (trim(do_statement) == "do()") then
                do_toggle = .true.
                do_statement = ""
            end if
        end if
        
        if (read_state == 2) then ! compute
            ! part 1
            isum_1 = isum_1 + (num_1 * num_2)
            ! part 2
            if (do_toggle == .true.) then
                isum_2 = isum_2 + (num_1 * num_2)
            end if
            read_state = 0
        end if
        
    end do

    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
end program day_3