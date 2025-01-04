program day_9
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n, i, j, position, file_left, file_right, space_left
    integer(int64) :: isum_1, isum_2
    integer, target, dimension(:), allocatable :: disk_map, disk_map2
    integer, pointer, dimension(:) :: files, space
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    character(len=1), dimension(:,:), allocatable :: antenna_map
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input09.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    inquire(io, size = n)
    allocate(disk_map(n), disk_map2(n))
    do i = 1, n
        read(io, "(I1)", advance = "no") disk_map(i)
        disk_map2(i) = disk_map(i)
    end do
    close(io)
    
    files(0:) => disk_map(1::2)
    space(0:) => disk_map(2::2)
    
    position = 0
    file_left = 0
    file_right = size(files) - 1
    space_left = 0
    isum_1 = 0
    outer: do
        do i = 1, files(file_left)
            isum_1 = isum_1 + position * file_left
            position = position + 1
        end do
        files(file_left) = 0
        file_left = file_left + 1
        if (file_left >= size(files)) exit
        if (files(file_left) == 0) exit
        do i = 1, space(space_left)
            do
                if (files(file_right) == 0) then
                    file_right = file_right - 1
                    if (file_right <= -1) exit outer
                else
                files(file_right) = files(file_right) - 1
                isum_1 = isum_1 + position * file_right
                position = position + 1
                exit
                end if
            end do
        end do
        space_left = space_left + 1
    end do outer
    write(*,*), "Part 1: ", isum_1
    deallocate(disk_map)
    
    block
        type pat
            integer:: id, num
        end type pat
        type(pat), allocatable :: p(:)
        integer :: nid, idx
      
        files(0:) => disk_map2(1::2)
        nid = size(files) - 1
        p = [pat(0, disk_map2(1)), (pat(-1, disk_map2(i)), pat(i/2, disk_map2(i + 1)), i = 2, n, 2)]
        do i = nid, 0, -1
            idx = findloc(p%id, i, dim = 1)
            do j = 1, size(p)
                if (p(j)%id < 0 .and. p(j)%num >= files(i)) then
                    if(idx < j) exit
                    p(idx) = pat(-1, files(i))
                    if (p(j)%num - files(i) == 0) then
                        p(j) = pat(i, files(i))
                    else
                        p = [ p(1:j - 1), pat(i, files(i)), pat(-1, p(j)%num - files(i)), p(j + 1:) ]
                    end if
                    exit
                end if
            end do
        end do
        
        isum_2 = 0
        position = 0
        do i = 1, size(p)
            if (p(i)%id < 0) then
                position = position + p(i)%num
            else
                do j = 1, p(i)%num
                    isum_2 = isum_2 + position * p(i)%id
                    position = position + 1
                end do
            end if
        end do
        write(*,*) "Part 2: ", isum_2
    end block
    deallocate(disk_map2)
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
        
end program day_9