program day_10
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, isum_1, isum_2, idx(2)
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(:,:), allocatable :: top_map, flags
    logical, dimension(:,:), allocatable :: unseen_head
    character(len=512) :: msg, line
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input10.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(top_map(n_lines, n_cols))
    do i = 1, n_lines
        read(io, "(A)") line
        do j = 1, n_cols
            read(line(j:j), "(I1)") top_map(i, j)
        end do
    end do
    close(io)
    allocate(unseen_head(n_lines, n_cols), source = .true.)
    allocate(flags(n_lines, n_cols))
    
    isum_1 = 0
    isum_2 = 0
    do
        flags = 0
        idx = findloc(top_map, 0, mask = unseen_head)
        if (idx(1) == 0 .and. idx(2) == 0) exit
        unseen_head(idx(1), idx(2)) = .false.
        call dfs(idx, top_map, flags)
        isum_1 = isum_1 + count(flags /= 0)
        isum_2 = isum_2 + sum(flags)
    end do
    deallocate(top_map, unseen_head, flags)
    
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2
    
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
    
    recursive subroutine dfs(idx, top_map, flags)
        integer, dimension(2), intent(in) :: idx
        integer, dimension(:,:), allocatable, intent(in) :: top_map
        integer, dimension(:,:), allocatable, intent(inout) :: flags
        integer, dimension(2) :: idy
        integer :: i
        integer, parameter :: move(2,4) = reshape([-1, 0, 0, 1, 1, 0, 0, -1], [2, 4])
        if (top_map(idx(1), idx(2)) == 9) then
            flags(idx(1), idx(2)) = flags(idx(1), idx(2)) + 1
            return
        end if
        do i = 1, size(move, dim = 2)
            idy = idx + move(:, i)
            if (out_of_bounds(idy, size(top_map, dim = 1), size(top_map, dim = 2))) cycle
            if (top_map(idy(1), idy(2)) == top_map(idx(1), idx(2)) + 1) call dfs(idy, top_map, flags)
        end do
    end subroutine dfs
    
    logical function out_of_bounds(coord, height, width) result(res)
        integer, dimension(2), intent(in) :: coord
        integer, intent(in) :: height, width
        if (coord(1) < 1 .or. coord(2) < 1 .or. coord(1) > height .or. coord(2) > width) then
            res = .true.
            return
        end if
        res = .false.
    end function out_of_bounds
    
end program day_10