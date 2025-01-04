program day_18
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, max_bytes, i, num_of_steps, lower_bytes, upper_bytes, mid
    integer, parameter :: num_of_bytes = 1024
    integer(int64) :: start_count, stop_count, rate
    integer, parameter :: memory_len = 70
    integer, dimension(:,:), allocatable :: memory_map
    integer, dimension(:,:), allocatable :: corruptions
    character(len=512) :: msg, line
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input18.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    max_bytes = number_of_lines_in_file(io)
    allocate(corruptions(max_bytes, 2))
    do i = 1, max_bytes
        if (stat /= 0) exit
        read(io, *, iostat = stat) corruptions(i, :)
    end do
    close(io)
    allocate(memory_map(0 : memory_len, 0 : memory_len))
    call count_steps_bfs(memory_map, memory_len, corruptions, num_of_bytes, num_of_steps)
    write(*,*) "Part 1: ", num_of_steps
    
    lower_bytes = num_of_bytes
    upper_bytes = max_bytes
    do ! binary search
        mid = (lower_bytes + upper_bytes) / 2
        call count_steps_bfs(memory_map, memory_len, corruptions, mid, num_of_steps)
        if (num_of_steps == -1) upper_bytes = mid
        if (num_of_steps > 0) lower_bytes = mid
        if (upper_bytes - lower_bytes == 1) exit
    end do
    write(*,*) "Part 2: ", corruptions(mid, :)
    
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
    
    
    subroutine count_steps_bfs(arr, n, cors, nbytes, nsteps)
        integer, intent(inout) :: arr(0:n, 0:n)
        integer, intent(in) :: n, nbytes
        integer, intent(out) :: nsteps
        integer, dimension(:,:), intent(in) :: cors
        integer :: i, j, k, npos, ntmp, ipos, itmp
        integer, dimension(5000, 2) :: pos, tmp
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        logical :: found_exit
        
        arr(:,:) = -2 ! -2 is safe, insert corruptions as -1
        do i = 1, nbytes
            arr(cors(i, 1),cors(i, 2)) = -1
        end do
        npos = 1
        pos(1, :) = [0, 0]
        nsteps = 0
        found_exit = .false.
        do 
            ntmp = 0
            do ipos = 1, npos
                i = pos(ipos, 1)
                j = pos(ipos, 2)
                arr(i, j) = nsteps
                if (i == n .and. j == n) then 
                    found_exit = .true.
                    exit
                end if
                do k = 1, size(move, dim = 2)
                    if (is_in_bounds([i, j] + move(:, k), 0, n)) then
                        if (arr(i + move(1, k), j + move(2, k)) == -2) then
                            do itmp = 1, ntmp
                                if (all(tmp(itmp, :) == [i, j] + move(:, k))) exit
                            end do
                            if (itmp > ntmp) ntmp = ntmp + 1
                            tmp(itmp, :) = [i, j] + move(:, k)
                        end if
                    end if
                end do
            end do
            if (found_exit .or. ntmp == 0) exit
            nsteps = nsteps + 1
            npos = ntmp
            pos(:npos, :) = tmp(:ntmp, :)
        end do
        if (.not. found_exit) nsteps = -1
    end subroutine
    
    logical function is_in_bounds(num, lower, upper) result(res)
        integer, dimension(2), intent(in) :: num
        integer, intent(in) :: lower, upper
        if (any(num < lower) .or. any(num > upper)) then
            res = .false.
            return
        end if
        res = .true.
    end function is_in_bounds
end program day_18