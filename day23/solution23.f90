program day23
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, index_2, index_1, id, num_of_cpu, num_of_lan
    integer:: cpu_1, cpu_2, isum_1, isum_2, nb_1, nb_2, id_1, id_2, max_ping, cpu, nb
    integer, dimension(16) :: lan
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: line, msg
    integer, dimension(:,:), allocatable :: network
    integer, dimension(:), allocatable :: cpu_to_idx, idx_to_cpu, ping, ping_tmp
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input23.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    allocate(network(0 : 16, 0 : 26 ** 2), source = 0)
    n_lines = number_of_lines_in_file(io)
    do i = 1, n_lines
        read(io, "(A)") line
        cpu_1 = cpu_to_id(line(1:2))
        cpu_2 = cpu_to_id(line(4:5))
        network(0, cpu_1) = network(0, cpu_1) + 1 ! Neighbour index
        network(network(0, cpu_1), cpu_1) = cpu_2 ! Set the neighbour entry
        network(0, cpu_2) = network(0, cpu_2) + 1 ! Repeat
        network(network(0, cpu_2), cpu_2) = cpu_1 ! Repeat
    end do
    close(io)
    
    isum_1 = 0
    do id = 19 * 26, 19 * 26 + 25 ! Sequences that start with "t", 19th letter of alphabet
        do nb_1 = 1, network(0, id) - 1
            cpu_1 = network(nb_1, id)
            if (network(0, cpu_1) <= 1) cycle ! Only one neighbour
            if (cpu_1 / 26 == 19 .and. cpu_1 < id) cycle ! Already done
            do nb_2 = nb_1 + 1, network(0, id)
                cpu_2 = network(nb_2, id)
                if (network(0, cpu_2) <= 1) cycle ! Only one neighbour
                if (cpu_2 / 26 == 19 .and. cpu_2 < id) cycle ! Already done
                if (all(network(1 : network(0, cpu_2), cpu_2) /= cpu_1)) cycle ! Check mutual neighbours
                isum_1 = isum_1 + 1
            end do
        end do
    end do
    write(*,*) "Part 1: ", isum_1
    
    num_of_cpu = 0
    allocate(cpu_to_idx(0 : 26 ** 2), source = -1)
    do id = 0, 26 ** 2
        if (network(0, id) == 0) cycle
        num_of_cpu = num_of_cpu + 1
        cpu_to_idx(id) = num_of_cpu
    end do
    
    num_of_cpu = 0
    allocate(idx_to_cpu(num_of_cpu)) ! Left inverse function
    do id = 0, 26 ** 2
        if (cpu_to_idx(id) == -1) cycle
        num_of_cpu = num_of_cpu + 1
        idx_to_cpu(num_of_cpu) = id
    end do
    
    allocate(ping(num_of_cpu), source = 0)
    allocate(ping_tmp(num_of_cpu), source = 0)
    max_ping = 0
    lan(:) = 0
    num_of_lan = 0
    do index_2 = 1, num_of_cpu
        ping(:) = 0
        ping(index_2) = 1
        id_2 = idx_to_cpu(index_2)
        do i = 1, 3
            ping_tmp(:) = 0
            do index_1 = 1, num_of_cpu
                if (ping(index_1) == 0) cycle
                id_1 = idx_to_cpu(index_1) ! Ping all neighbours
                if (id_1 /= id_2 .and. all(network(1 : network(0, id_1), id_1) /= id_2)) cycle
                do nb = 1, network(0, id_1)
                    cpu = network(nb, id_1)
                    ping_tmp(cpu_to_idx(cpu)) = ping_tmp(cpu_to_idx(cpu)) + ping(index_1)
                end do
            end do
            ping(:) = ping_tmp(:)
        end do
        if (maxval(ping) > max_ping) then 
            num_of_lan = 1
            lan(1) = id_2
            max_ping = maxval(ping)
        else if (maxval(ping) == max_ping) then 
            num_of_lan = num_of_lan + 1
            lan(num_of_lan) = id_2
        end if
    end do
    
    line = ""
    do i = 1, num_of_lan
        line = trim(line) // id_to_cpu(lan(i)) // ","
    end do
    line(len_trim(line):len_trim(line)) = ""
    write(*,*) "Part 2: ", trim(line)

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
    
    integer function cpu_to_id(node) result(id)
        character(len=2), intent(in) :: node
        integer, dimension(2) :: tmp
        tmp = [iachar(node(1:1)) - 97, iachar(node(2:2)) - 97]
        id = 26 * tmp(1) + tmp(2)
    end function cpu_to_id
    
    character(len=2) function id_to_cpu(i)
        integer, intent(in) :: i
        id_to_cpu(1:1) = achar(i / 26 + 97)
        id_to_cpu(2:2) = achar(modulo(i, 26) + 97)
        return
    end function id_to_cpu
    
end program day23