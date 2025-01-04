program day_17
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, num_of_cmds, pointer, cmd_len
    integer(int64) :: k, upper_bound, lower_bound
    integer(int64), dimension(3) :: reg
    integer, dimension(:), allocatable :: cmd_line, test_line
    integer, dimension(:,:), allocatable :: cmd
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line, output
    abstract interface
        subroutine op(n)
        integer, intent(in) :: n
        end subroutine op
    end interface
    type instruction_to_code
        procedure(op), nopass, pointer :: code
    end type instruction_to_code
    type(instruction_to_code), dimension(0:7) :: instructions
        instructions(0)%code => adv
        instructions(1)%code => bxl
        instructions(2)%code => bst
        instructions(3)%code => jnz
        instructions(4)%code => bxc
        instructions(5)%code => out
        instructions(6)%code => bdv
        instructions(7)%code => cdv
        
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input17.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    do i = 1, 3
        read(io, "(A)") line
        read(line(13:), *) reg(i)
    end do
    read(io, "(A1)")
    read(io, "(A)") line
    line = line(10:)
    num_of_cmds = (len_trim(line) + 1) / 4
    allocate(cmd(2, 0 : num_of_cmds - 1))
    read(line, *) cmd
    call run_program(reg, cmd)
    write(*,*) "Part 1: ", trim(output)
    
    cmd_line = string_to_integers(line, ",")
    
    lower_bound = 8_int64 ** (size(cmd_line) - 1)
    upper_bound = 8_int64 ** size(cmd_line)
    
    cmd_len = size(cmd_line)
    
    do i = 0, size(cmd_line) - 8
        write(*,*) "Step ", i
        do k = lower_bound, upper_bound - 1, 8_int64 ** (15 - i)
            reg = [k, 0_int64, 0_int64]
            call run_program(reg, cmd)
            test_line = string_to_integers(output, ",")
            if (all(test_line(cmd_len - i:) == cmd_line(cmd_len - i:))) then
                write(*,*) "found", k, test_line
                lower_bound = k
                upper_bound = k + 8_int64 ** (15 - i)
            end if
        end do
    end do
    
    do k = lower_bound, upper_bound - 1, 1
        reg = [k, 0_int64, 0_int64]
        call run_program(reg, cmd)
        test_line = string_to_integers(output, ",")
        if (all(test_line == cmd_line)) then
            write(*,*) "found", k
            exit
        end if
    end do
    
    deallocate(cmd)
    
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
            if (str(i:i)==sep) then
                n_sep = n_sep + 1
                str(i:i) = ','
            end if
        end do
        allocate(a(n_sep+1))
        read(str,*) a
    end function
    
    subroutine run_program(reg, cmd)
        integer(int64), dimension(3), intent(inout) :: reg
        integer, dimension(:,0:), intent(in) :: cmd
        integer :: x, y
        pointer = 0
        output = ""
        do
            x = cmd(1, pointer)
            y = cmd(2, pointer)
            call instructions(x)%code(y)
            if (pointer >= num_of_cmds) exit
        end do
        output = output(: len_trim(output) - 1)
    end subroutine run_program
    
    subroutine adv(n)
        integer, intent(in) :: n
        reg(1) = reg(1) / (2 ** operand(n))
        pointer = pointer + 1
    end subroutine adv

    subroutine bxl(n)
        integer, intent(in) :: n
        reg(2) = xor(reg(2), n)
        pointer = pointer + 1
    end subroutine bxl

    subroutine bst(n)
        integer, intent(in) :: n
        reg(2) = modulo(operand(n), 8)
        pointer = pointer + 1
    end subroutine bst

    subroutine jnz(n)
        integer,intent(in) :: n
        if (reg(1) == 0) then
            pointer = pointer + 1
            return
        end if
        pointer = operand(n)
    end subroutine jnz
    
    subroutine bxc(n)
        integer,intent(in)::n
        reg(2) = xor(reg(2), reg(3))
        pointer = pointer + 1
    end subroutine bxc

    subroutine out(n)
        integer, intent(in) :: n
        character(len=32) :: str
        write(str, "(I1)") modulo(operand(n), 8)
        output = trim(output) // trim(str) // ","
        pointer = pointer + 1
    end subroutine out

    subroutine bdv(n)
        integer, intent(in) :: n
        reg(2) = reg(1) / (2 ** operand(n))
        pointer = pointer + 1
    end subroutine bdv

    subroutine cdv(n)
        integer, intent(in) :: n
        reg(3) = reg(1) / (2 ** operand(n))
        pointer = pointer + 1
    end subroutine cdv

    integer function operand(n) result(res)
        integer, intent(in) :: n
        select case(n)
            case(0:3); res = n
            case(4); res = reg(1)
            case(5); res = reg(2)
            case(6); res = reg(3)
            case default
                error stop "Invalid combo operand"
        end select
    end function operand
    
end program day_17