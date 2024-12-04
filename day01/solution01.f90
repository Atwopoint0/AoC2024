program day_1
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, i, line_1, line_2, distance, similarity
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg
    integer, dimension(:), allocatable :: group_1, group_2
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input01.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = 0
    do
        read(io, *, iostat = stat)
        if (stat /= 0) then
            exit
        end if
        n_lines = n_lines + 1
    end do
    allocate(group_1(n_lines), group_2(n_lines))
    rewind(io)
    
    do i = 1, n_lines
        read(io, *, iostat = stat) line_1, line_2
        group_1(i) = line_1
        group_2(i) = line_2
    end do
    close(io)
    
    call sort(group_1)
    call sort(group_2)
    
    ! part 1
    distance = 0
    do i = 1, n_lines
        distance = distance + abs(group_2(i) - group_1(i))
    end do
    write(*,*) "Part 1: ", distance
    
    ! part 2
    similarity = 0
    do i = 1, n_lines
        similarity = similarity + group_1(i) * count(group_1(i) == group_2)
    end do
    write(*,*) "Part 2: ", similarity
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    subroutine sort(ivec) ! simple insertion sort
        integer :: i, j, ipivot, itemp
        integer, dimension(:), intent(out) :: ivec
        do i = 1, size(ivec)
            do j = i, 2, -1
                if (ivec(j) < ivec(j-1)) then
                    itemp = ivec(j)
                    ivec(j) = ivec(j-1)
                    ivec(j-1) = itemp
                else
                    exit
                end if
            end do
        end do
    end subroutine sort
    
end program day_1