program day16
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, min_len
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: line, msg
    character(len=1), dimension(:,:), allocatable :: maze
    type node
        integer :: lens
        integer, dimension(2) :: idx
        integer :: mv
    end type node
    integer, dimension(2) :: idx
    integer, dimension(:,:,:), allocatable :: flags
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input16.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(maze(n_lines, n_cols))
    allocate(flags(4, n_lines, n_cols))
    do i = 1, n_lines
        read(io, "(A)") line
        maze(i, :) = [(line(j:j), j = 1, n_cols)]
    end do
    close(io)
    idx = findloc(maze, "S")
    call dijkstra(idx, maze, flags, min_len)
    deallocate(maze, flags)
    write(*,*) "Part 1: ", min_len
    
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
            read(iunit, fmt = "(A1)", iostat = istat) tmp
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
        n_cols = len_trim(line)
        rewind(iunit)
    end function number_of_cols_in_file
    
    subroutine dijkstra(idx, arr, flags, min_len)
        use heap_mod
        integer, dimension(2), intent(in) :: idx
        character(len=1), dimension(:,:), intent(in) :: arr
        integer, dimension(:,:,:), allocatable, intent(inout) :: flags
        integer, intent(inout) :: min_len
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        integer, parameter :: rotate(2, 4) = reshape([3, 4, 3, 4, 1, 2, 1, 2], [2, 4])
        type(heap) :: m
        type(node) :: x
        integer :: id
        integer, dimension(2) :: idy
        call m%init(size(arr, dim = 1) * size(arr, dim = 2) * 4, node(0, [0, 0], 0), equals, compare, swap)
        call m%insert(node(0, idx, 4))
        call m%insert(node(1000, idx, 1))
        call m%insert(node(1000, idx, 2))
        call m%insert(node(2000, idx, 3))
        flags = 0
        do while(m%size > 0)
            call m%pop(x)
            id = flags(x%mv, x%idx(1), x%idx(2))
            if (id /= 0) cycle
            flags(x%mv, x%idx(1), x%idx(2)) = 1
            idy = x%idx + move(:, x%mv)
            select case(arr(idy(1), idy(2)))
            case("#")
            case(".")
                call m%insert(node(x%lens+1, idy, x%mv))
                call m%insert(node(x%lens + 1001, idy, rotate(1, x%mv)))
                call m%insert(node(x%lens + 1001, idy, rotate(2, x%mv)))
            case("E")
                min_len = x%lens + 1
                exit
            end select
        end do
    end subroutine dijkstra

    subroutine equals(a, b)
        class(*), intent(inout) :: a
        class(*), intent(in) :: b
        select type(a)
            class is (node)
            select type(b)
                class is (node)
                a%lens = b%lens
                a%idx = b%idx
                a%mv = b%mv
            end select
        end select
    end subroutine equals

    subroutine swap(a, b)
        class(*), intent(inout) :: a, b
        type(node) :: tmp
        call equals(tmp, b)
        call equals(b, a)
        call equals(a, tmp)
    end subroutine swap

    logical function compare(a, b) result(res)
        class(*), intent(in) :: a, b
        select type(a)
            class is (node)
            select type(b)
                class is (node)
                res = a%lens < b%lens
            end select
        end select 
    end function compare
end program day16