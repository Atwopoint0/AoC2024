program day_12
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, k, isum_1, isum_2, length, area, tag, corners
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(:,:), allocatable :: flags
    integer, dimension(:,:,:), allocatable :: flags2
    character(len=512) :: msg, line
    character(len=1), dimension(:,:), allocatable :: garden_map
    
    call system_clock(count = start_count, count_rate = rate)
    
    open(newunit = io, file = "input12.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(garden_map(n_lines, n_cols))
    allocate(flags(n_lines, n_cols), source = 0)
    allocate(flags2(4, n_lines, n_cols), source = 0)
    do i = 1, n_lines
        read(io, "(A)") line
        garden_map(i, :) = [(line(j:j), j = 1, n_cols)]
    end do
    
    isum_1 = 0
    do i = 1, n_lines
        do j = 1, n_cols
            if (flags(i, j) == 0) then
                length = 0
                area = 1
                tag = tag + 1 ! keep regions unique
                flags(i, j) = tag
                call dfs([i, j], tag, area, length, garden_map, flags)
                isum_1 = isum_1 + area * length
            end if
        end do
    end do
    isum_2 = 0
    do k = 1, tag
        corners = 0
        do i = 1, n_lines
            do j = 1, n_cols
                if (flags(i, j) == k) then
                    call side([i, j], corners, garden_map, flags2)
                end if
            end do
        end do
        isum_2 = isum_2 + count(flags == k) * corners
    end do
    
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2
    close(io)

    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    integer function number_of_lines_in_file(iunit) result(res)
        integer, intent(in) :: iunit
        integer :: n_lines
        character(len=1) :: tmp
        integer :: istat
        rewind(iunit)
        res = 0
        do
            read(iunit,fmt='(A1)',iostat=istat) tmp
            if (is_iostat_end(istat)) exit
            res = res + 1
        end do
        rewind(iunit)
    end function number_of_lines_in_file
    
    integer function number_of_cols_in_file(iunit) result(res)
        integer, intent(in) :: iunit
        integer :: istat
        character(len=512) :: line
        rewind(iunit)
        res = 0
        read(io, "(A)") line
        res = len(trim(line))
        rewind(iunit)
    end function number_of_cols_in_file
    
    recursive subroutine dfs(idx, tag, area, length, arr, flags)
        integer, intent(in) :: idx(2), tag
        character(len=1), dimension(:,:), allocatable, intent(in) :: arr
        integer, dimension(:,:), allocatable, intent(inout) :: flags
        integer, intent(inout) :: area, length
        integer :: idy(2), i
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, 1, 0, -1], [2, 4])
        do i = 1, size(move, dim = 2)
            idy = idx + move(:, i)
            if (is_out_of_bounds(idy, size(arr, dim = 1), size(arr, dim = 2))) then ! map boundary
                length = length + 1
            else if (garden_map(idy(1), idy(2)) /= garden_map(idx(1), idx(2))) then ! region boundary
                length = length + 1
            else
                if (flags(idy(1), idy(2)) == tag) cycle
                flags(idy(1), idy(2)) = tag
                area = area + 1
                call dfs(idy, tag, area, length, arr, flags)
            end if
        end do
    end subroutine dfs
    
    logical function is_out_of_bounds(point, height, width) result(res)
        integer, dimension(2), intent(in) :: point
        integer, intent(in) :: height, width
        if (point(1) < 1 .or. point(2) < 1 .or. point(1) > height .or. point(2) > width) then
            res = .true.
            return
        end if
        res = .false.
    end function is_out_of_bounds
    
    subroutine side(idx, corners, arr, flags2)
        integer, intent(in) :: idx(2)
        character(len=1), dimension(:,:), allocatable, intent(in) :: arr
        integer, dimension(:,:,:), allocatable, intent(inout) :: flags2
        integer, intent(inout) :: corners
        integer :: idy(2), i
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, 1, 0, -1], [2, 4])
        do i = 1, size(move, dim = 2)
            idy = idx + move(:, i)
            if (is_out_of_bounds(idy, size(arr, dim = 1), size(arr, dim = 2))) then ! map boundary
                if (.not. is_same_side(idx, i, 1, arr, flags2)) corners = corners + 1
            else if (arr(idy(1),idy(2)) /= arr(idx(1),idx(2))) then ! region boundary
                if (.not. is_same_side(idx, i, 2, arr, flags2)) corners = corners + 1
            end if
            flags2(i, idx(1), idx(2)) = 1
        end do
    end subroutine side

    logical function is_same_side(idx, mv, type, arr, flags2) result(res)
        integer,intent(in) :: idx(2), mv, type
        character(len=1), dimension(:,:), allocatable, intent(in) :: arr
        integer, dimension(:,:,:), allocatable, intent(inout) :: flags2
        integer:: idy(2), i, j
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, 1, 0, -1], [2, 4])
        integer, parameter :: map(2, 4) = reshape([3, 4, 3, 4, 1, 2, 1, 2], [2, 4])
        res = .false.
        do i = 1, 2
            idy = idx + move(:, map(i, mv))
            if (is_out_of_bounds(idy, size(arr, dim = 1), size(arr, dim = 2))) cycle
            if (arr(idy(1), idy(2)) /= arr(idx(1), idx(2))) cycle
            if (flags2(mv, idy(1), idy(2)) == 0) cycle
            idy = idy + move(:, mv)
            if (type == 1) then
                res = is_out_of_bounds(idy, size(arr, dim = 1), size(arr, dim = 2))
                if (res) return
            else
                if (is_out_of_bounds(idy, size(arr, dim = 1), size(arr, dim = 2))) cycle
                res = arr(idy(1), idy(2)) /= arr(idx(1), idx(2))
                if (res) return
            end if
        end do
    end function is_same_side
    
end program day_12