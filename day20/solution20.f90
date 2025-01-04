program day20
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, n_cols, i, j, len_track, isum_1, isum_2
    integer, dimension(2) :: start, finish
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: line, msg
    integer, dimension(2), parameter :: cheat_size = [2, 20]
    integer, parameter :: min_gain = 100
    integer, dimension(:,:), allocatable :: race_map, track
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input20.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    n_lines = number_of_lines_in_file(io)
    n_cols = number_of_cols_in_file(io)
    allocate(race_map(n_lines, n_cols))
    do i = 1, n_lines
        read(io, "(A)") line
        do j = 1, n_cols
            select case (line(j:j))
                case ("#") ; race_map(i, j) = -1 ! Wall
                case (".") ; race_map(i, j) = -2 ! Empty
                case ("S") ; race_map(i, j) = -2; start = [i, j]
                case ("E") ; race_map(i, j) = -2; finish = [i, j]
            end select
        end do
    end do
    close(io)
    
    allocate(track(count(race_map(:,:) == -2), 2), source = 0)
    call find_track(race_map, track, len_track, start, finish)
    
    isum_1 = find_cheats(race_map, track, len_track, cheat_size(1), min_gain)
    write(*,*) "Part 1: ", isum_1
    isum_2 = find_cheats(race_map, track, len_track, cheat_size(2), min_gain)
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
            read(iunit, fmt = '(A1)', iostat = istat) tmp
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
    
    subroutine find_track(race_map, track, len_track, start, finish)
        implicit none
        integer, dimension(:,:), intent(inout) :: race_map, track
        integer, intent(out) :: len_track
        integer, dimension(2), intent(in) :: start, finish
        integer, parameter :: move(2, 4) = reshape([-1, 0, 1, 0, 0, -1, 0, 1], [2, 4])
        integer :: i, j, k
        i = start(1)
        j = start(2)
        len_track = 0
        do
            len_track = len_track + 1
            race_map(i, j) = len_track
            track(len_track, :) = [i, j]
            if (finish(1) == i .and. finish(2) == j) exit
            do k = 1, size(move, dim = 2)
                if (race_map(i + move(1, k), j + move(2, k)) == -2) then
                    i = i + move(1, k)
                    j = j + move(2, k)
                    exit
                end if                
            end do
        end do
    end subroutine find_track
    
    integer function find_cheats(race_map, track, len_track, cheat_size, min_gain) result(num_cheat)
        integer, dimension(:,:), intent(in) :: race_map, track
        integer, intent(in) :: len_track, cheat_size, min_gain
        integer :: i, j, k, l, n, idx, idy, cost
        num_cheat = 0
        do n = 1, len_track
            i = track(n, 1)
            j = track(n, 2)
            idx = race_map(i, j)
            do k = - cheat_size, cheat_size
                if (is_out_of_bounds(i + k, 1, size(race_map, dim = 1))) cycle
                do l = - (cheat_size - abs(k)), cheat_size - abs(k)
                    if (is_out_of_bounds(j + l, 1, size(race_map, dim = 2))) cycle
                    cost = abs(k) + abs(l)
                    if (cost < 2) cycle ! Useless cheat
                    idy = race_map(i + k, j + l)
                    if (idy == -1) cycle ! Wall
                    if (idy < idx + cost + min_gain) cycle ! Bad cheat
                    num_cheat = num_cheat + 1
                end do
            end do
        end do
    end function find_cheats
    
    logical function is_out_of_bounds(num, low, high) result(res)
        integer, intent(in) :: num, low, high
        if (num > high .or. num < low) then
            res = .true.
            return
        end if
        res = .false.
        return
    end function is_out_of_bounds
    
end program day20