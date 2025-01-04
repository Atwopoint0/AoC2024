program day_8
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(2) :: idx
    character(len=512) :: msg, line
    character(len=1), dimension(:,:), allocatable :: antenna_map
    integer, dimension(:,:), allocatable :: flags1, flags2
    logical, dimension(:,:), allocatable :: is_antenna
    type point
        integer :: location(2)
    end type point
    type(point), dimension(:), allocatable :: points
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input08.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(antenna_map(n_lines, n_cols))
    allocate(is_antenna(n_lines, n_cols), source = .true.)
    allocate(flags1(n_lines, n_cols), source = 0)
    allocate(flags2(n_lines, n_cols), source = 0)
    do i = 1, n_lines
        read(io, "(A)") line
        antenna_map(i, :) = [(line(j:j), j = 1, n_cols)]
    end do
    close(io)
    
    do i = 1, n_lines
        do j = 1, n_cols
            if (antenna_map(i, j) == ".") cycle
            if (.not. is_antenna(i, j)) cycle
            points = [point([i, j])]
            is_antenna(i ,j) = .false.
            do
                idx = findloc(antenna_map, antenna_map(i, j), is_antenna)
                if (all(idx == 0)) exit
                points = [points, point(idx)]
                is_antenna(idx(1), idx(2)) = .false.
            end do
            call find_antinodes(points, n_lines, n_cols, flags1, resonate = .false.)
            call find_antinodes(points, n_lines, n_cols, flags2, resonate = .true.)
        end do
    end do
    
    write(*,*) "Part 1: ", count(flags1 == 1)
    write(*,*) "Part 2: ", count(flags2 == 1)
    
    deallocate(antenna_map, is_antenna, flags1, flags2)
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
    
    subroutine find_antinodes(nodes, height, width, found_antinodes, resonate)
        integer, intent(in) :: height, width
        integer :: i, j, k, antinode(2)
        integer, dimension(:,:), allocatable, intent(inout) :: found_antinodes
        type(point), dimension(:), allocatable, intent(in) :: nodes
        logical, intent(in) :: resonate
        if (size(nodes) == 1) return
        do i = 1, size(nodes)
            do j = i + 1, size(nodes)
                associate(node_i => points(i)%location, node_j => points(j)%location)
                    k = 0
                    do
                        if (.not. resonate) k = 1
                        antinode = node_i + k * (node_i - node_j)
                        if (is_out_of_bounds(antinode, height, width)) exit
                        found_antinodes(antinode(1), antinode(2)) = 1
                        if (.not. resonate) exit
                        k = k + 1
                    end do
                    k = 0
                    do
                        if (.not. resonate) k = 1
                        antinode = node_j + k * (node_j - node_i)
                        if (is_out_of_bounds(antinode, height, width)) exit
                        found_antinodes(antinode(1), antinode(2)) = 1
                        if (.not. resonate) exit
                        k = k + 1
                    end do
                end associate
            end do
        end do
    end subroutine find_antinodes
    
    logical function is_out_of_bounds(point, height, width) result(res)
        integer, intent(in) :: width, height
        integer, dimension(2), intent(in) :: point
        if (point(1) < 1 .or. point(1) > height .or. point(2) < 1 .or. point(2) > width) then
            res = .true.
            return
        end if
        res = .false.
    end function is_out_of_bounds
    
end program day_8