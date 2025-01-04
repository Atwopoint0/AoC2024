program day25
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, j, n_keys, n_locks, isum_1
    integer, dimension(5) :: mesh
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(512, 5) :: keys, locks
    character(len=512) :: msg, line
    character(len=5), dimension(7) :: schematic
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input25.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    n_keys = 0; n_locks = 0
    do
        if (stat /= 0) exit
        do i = 1, 7
            read(io, "(A)", iostat = stat) schematic(i)
        end do
        if (schematic(1)(1:1) == "#") then ! Lock
            n_locks = n_locks + 1
            do i = 2, 6
                do j = 1, 5
                    if (schematic(i)(j:j) == "#") then
                        locks(n_locks, j) = locks(n_locks, j) + 1
                    end if
                end do
            end do
        else ! Key
            n_keys = n_keys + 1
            do i = 2, 6
                do j = 1, 5
                    if (schematic(i)(j:j) == "#") then
                        keys(n_keys, j) = keys(n_keys, j) + 1
                    end if
                end do
            end do
        end if
        read(io, "(A1)", iostat = stat)
    end do
    close(io)
    isum_1 = 0
    do i = 1, n_keys
        do j = 1, n_locks
            mesh = keys(i, :) + locks(j, :)
            if (all(mesh <= 5)) then
                isum_1 = isum_1 + 1
            end if
        end do
    end do
    write(*,*) isum_1
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
end program day25