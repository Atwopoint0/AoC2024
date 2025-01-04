program day_15
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, isum_1, isum_2
    integer(int64) :: start_count, stop_count, rate
    integer, dimension(2) :: idx_1, idx_2
    character(len=1), dimension(:,:), allocatable :: warehouse_map, warehouse_map_x2
    character(len=512) :: msg, line
    character(len=1) :: mv
    type point
        integer, dimension(2) :: coordinate
        character :: box_side
    end type point
    type(point), dimension(:), allocatable :: path
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input15.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_grid(io)
    n_cols = number_of_cols_in_grid(io)
    allocate(warehouse_map(n_lines, n_cols))
    allocate(warehouse_map_x2(n_lines, 2 * n_cols))
    do i = 1, n_lines
        read(io, "(A)") line
        do j = 1, n_cols
            warehouse_map(i, j) = line(j:j)
            if (warehouse_map(i, j) == "O") then
                warehouse_map_x2(i, 2 * j - 1) = "["
                warehouse_map_x2(i, 2 * j) = "]"
            else if (warehouse_map(i, j) == "@") then
                warehouse_map_x2(i, 2 * j - 1) = "@"
                warehouse_map_x2(i, 2 * j) = "." 
            else
                warehouse_map_x2(i, 2 * j - 1) =  warehouse_map(i, j)
                warehouse_map_x2(i, 2 * j) = warehouse_map(i, j)
            end if
        end do
    end do
    read(io, "(A1)")
    
    idx_1 = findloc(warehouse_map, "@")
    idx_2 = findloc(warehouse_map_x2, "@")
    do
        read(io, "(A1)", advance = "no", iostat = stat) mv
        if (is_iostat_end(stat)) exit
        if (mv == "") cycle
        call update_map(idx_1, dir(mv), warehouse_map)
        call update_map_2(idx_2, dir(mv), warehouse_map_x2)
    end do
    close(io)
    
    isum_1 = 0
    isum_2 = 0
    do i = 1, n_lines
        do j = 1, n_cols
            if (warehouse_map(i, j) == "O") then
                isum_1 = isum_1 + 100 * (i - 1) + (j - 1)
            end if
        end do
    end do
    
    do i = 1, n_lines
        do j = 1, 2 * n_cols
            if (warehouse_map_x2(i, j) == "[") then
                isum_2 = isum_2 + 100 * (i - 1) + (j - 1)
            end if
        end do
    end do
    
    write(*,*) "Part 1: ", isum_1
    write(*,*) "Part 2: ", isum_2
    
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    function number_of_lines_in_grid(iunit) result(n_lines)
        integer, intent(in) :: iunit
        integer :: n_lines
        character(len=1) :: tmp
        integer :: istat
        rewind(iunit)
        n_lines = 0
        do
            read(iunit, fmt = '(A1)', iostat = istat) tmp
            if (is_iostat_end(istat) .or. tmp == "") exit
            n_lines = n_lines + 1
        end do
        rewind(iunit)
    end function number_of_lines_in_grid
    
    function number_of_cols_in_grid(iunit) result(n_cols)
        integer, intent(in) :: iunit
        integer :: n_cols
        integer :: istat
        character(len=512) :: line
        rewind(iunit)
        n_cols = 0
        read(io, "(A)") line
        n_cols = len(trim(line))
        rewind(iunit)
    end function number_of_cols_in_grid
    
    pure integer function dir(mv) result(res)
        character(len=1), intent(in) :: mv
        select case(mv)
            case("^"); res = 1
            case("v"); res = 2
            case("<"); res = 3
            case(">"); res = 4
            case default; error stop "invalid"
            end select
            return
    end function dir
    
    subroutine update_map(idx, mv, arr)
        integer, intent(inout) :: idx(2)
        integer, intent(in) :: mv
        character(len=1), dimension(:,:), intent(inout) :: arr
        integer :: idy(2)
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        idy = idx
        do
            idy = idy + move(:, mv)
            select case(arr(idy(1), idy(2)))
            case("#"); exit
            case(".")
                arr(idy(1), idy(2)) = "O"
                arr(idx(1), idx(2)) = "."
                idx = idx + move(:, mv)
                arr(idx(1), idx(2)) = "@"
                exit
            end select
      end do
    end subroutine update_map
    
    subroutine update_map_2(idx, mv, arr)
        integer, intent(inout) :: idx(2)
        integer, intent(in) :: mv
        character(len=1), dimension(:,:), intent(inout) :: arr
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        integer :: i
        integer, dimension(2) :: idy, idz
        logical :: is_moveable_box
        logical, allocatable :: tag(:,:)
        
        allocate(tag(size(arr, dim = 1), size(arr, dim = 1) * 2))
        if (mv > 2) then
            idy = idx
            do
                idy = idy + move(:,mv)
                select case(arr(idy(1), idy(2)))
                case("#"); exit
                case(".")
                    do
                        if (all(idy == idx)) exit
                        idz = idy - move(:, mv)
                        arr(idy(1), idy(2)) = arr(idz(1), idz(2))
                        idy = idy - move(:, mv)
                    end do
                    arr(idx(1), idx(2)) = "."
                    idx = idx + move(:, mv)
                    arr(idx(1), idx(2)) = "@"
                    exit
                end select
            end do
        else
            idy = idx + move(:,mv)
            path = [point::]
            select case(arr(idy(1), idy(2)))
            case("[", "]")
                is_moveable_box = .true.
                call dfs(idy, mv, arr, is_moveable_box)
                if (.not. is_moveable_box) return
                tag = .true.
                do i = 1, size(path)
                    idz = path(i)%coordinate + move(:, mv)
                    arr(idz(1), idz(2)) = path(i)%box_side
                    tag(idz(1), idz(2)) = .false.
                end do
                do i = 1, size(path)
                    idy = path(i)%coordinate
                    if (tag(idy(1), idy(2))) then
                        arr(idy(1), idy(2)) = "."
                    end if
                end do
                arr(idx(1), idx(2)) = "."
                idx = idx + move(:, mv)
                arr(idx(1), idx(2)) = "@"
            case(".")
                arr(idx(1), idx(2)) = "."
                idx = idx + move(:, mv)
                arr(idx(1), idx(2)) = "@"
            end select
        end if
    end subroutine update_map_2
    
    recursive subroutine dfs(idx, mv, arr, tag)
        integer, dimension(2), intent(in) :: idx
        integer, intent(in) :: mv
        character(len=1), dimension(:,:), intent(in) :: arr
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        logical, intent(inout) :: tag
        select case(arr(idx(1), idx(2)))
        case("#")
            tag = .false.
            return
        case("[")
            path = [path, point(idx, "["), point(idx + [0, 1], "]")]
            call dfs(idx + move(:, mv), mv, arr, tag)
            call dfs(idx + [0, 1] + move(:, mv), mv, arr, tag)
        case("]")
            path = [path, point(idx, "]"), point(idx - [0, 1], "[")]
            call dfs(idx + move(:, mv), mv, arr, tag)
            call dfs(idx - [0, 1] + move(:, mv), mv, arr, tag)
        case("."); return
        end select
    end subroutine dfs
    
end program day_15