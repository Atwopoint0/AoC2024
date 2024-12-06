program day_6
    use, intrinsic :: iso_fortran_env
    implicit none
    ! should use complex numbers in future
    integer :: io, stat, n_lines, n_cols, i, j, k, dir, prev_dir, isum_1
    integer, dimension(2) :: idx0, idx1, idx2, prev_idx
    integer, dimension(:,:), allocatable :: flags
    integer(int64) :: start_count, stop_count, rate
    integer, parameter :: dirs(2,0:3) = reshape([-1,0,0,1,1,0,0,-1],[2,4])
    integer, dimension(:,:,:), allocatable :: visited
    integer, dimension(:,:), allocatable :: path
    character(len=512) :: msg, line
    character(len=1), dimension(:,:), allocatable :: guard_map
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input06.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    ! parse
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(guard_map(n_lines, n_cols))
    allocate(flags(n_lines, n_cols), source = 0)
    allocate(visited(n_lines, n_cols, 0:3), source = 0)
    allocate(path(n_lines**2, 3))
    do i = 1, n_lines
        read(io, "(A)") line
        do j = 1, n_cols
            guard_map(i,j) = line(j:j)
        end do
    end do
    close(io)
    
    ! part 1
    idx0 = findloc(guard_map, "^")
    idx1 = idx0
    dir = 0
    k = 1
    do
        idx2 = idx1 + dirs(:, dir)
        if (out_of_bounds_check(idx2, 1, n_lines, 1, n_cols)) exit
        if (guard_map(idx2(1), idx2(2)) == "#") then
            dir = mod(dir + 1, 4)
        else
            path(k,:) = [idx1(1), idx1(2), dir]
            idx1 = idx2
            flags(idx1(1), idx1(2)) = flags(idx1(1), idx1(2)) + 1
            k = k + 1
        end if
    end do
    write(*,*) "Part 1: ", count(flags /= 0)
    
    ! part 2
    flags = 0
    do i = 1, k - 2 ! only need to check the path from part 1
        if (flags(path(i + 1, 1), path(i + 1, 2)) >= 1) cycle ! avoid repeating nodes
        flags(path(i + 1, 1), path(i + 1, 2)) = 1
        idx1 = [path(i, 1), path(i, 2)] ! traverse from the blockage onwardss
        guard_map(path(i + 1, 1), path(i + 1, 2)) = "#"
        dir = path(i, 3)
        visited = 0
        do
            idx2 = idx1 + dirs(:, dir)
            if (out_of_bounds_check(idx2, 1, n_lines, 1, n_cols)) exit
            if (guard_map(idx2(1), idx2(2)) == "#") then
                if (visited(idx1(1), idx1(2), dir) == 1) then ! check corners for loop only
                    flags(path(i + 1, 1), path(i + 1, 2)) = 2
                    exit
                end if
                visited(idx1(1), idx1(2), dir) = 1
                dir = mod(dir + 1, 4)
            else
                idx1 = idx2
            end if
        end do
        guard_map(path(i + 1, 1), path(i + 1, 2)) = "."
    end do
    write(*,*) "Part 2: ", count(flags == 2)
    deallocate(guard_map, flags, visited, path)
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function number_of_cols_in_file(iunit) result(n_cols)
        integer, intent(in) :: iunit
        integer :: n_cols
        integer :: istat
        character(len=512) :: line
        rewind(iunit)
        n_cols = 0
        read(io, "(A)") line
        n_cols = len(trim(line))
        rewind(iunit)
    end function number_of_cols_in_file
    
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
    
    function out_of_bounds_check(coord, x1, x2, y1, y2) result(inside)
        integer, intent(in) :: x1, x2, y1, y2
        integer, dimension(2), intent(in) :: coord
        logical :: inside
        inside = .false.
        if (coord(1) < x1 .or. coord(1) > x2 .or. coord(2) < y1 .or. coord(2) > y2 ) then
            inside = .true.
        end if
    end function out_of_bounds_check

end program day_6