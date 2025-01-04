program day_14
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, sep_index, num_of_robots, i, t, compactness, idx, idy
    integer, parameter :: window = 33
    integer, dimension(2) :: pos
    integer, parameter :: time = 100
    integer, dimension(2), parameter :: box_size = [101, 103]
    integer, dimension(2,2) :: quadrant
    integer, dimension(0 : box_size(1) - 1, 0 : box_size(2) - 1) :: image
    integer, dimension(:,:), allocatable :: positions, velocities
    integer(int64) :: start_count, stop_count, rate
    character(len=512) :: msg, line
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input14.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    quadrant = 0
    num_of_robots = number_of_lines_in_file(io)
    allocate(positions(num_of_robots, 2))
    allocate(velocities(num_of_robots, 2))
    do i = 1, num_of_robots
        read(io, "(A)", iostat = stat) line
        sep_index = index(line, " ")
        read(line(3 : sep_index - 1), *) positions(i, :)
        read(line(sep_index + 3 : ), *) velocities(i, :)
        pos = modulo(positions(i, :) + time * velocities(i, :), box_size)
        if (any(pos == (box_size - 1) / 2)) then
            cycle
        end if
        pos = (sign(1, pos - (box_size - 1) / 2) + 3) / 2
        quadrant(pos(1), pos(2)) = quadrant(pos(1), pos(2)) + 1
    end do
    close(io)
    write(*,*) "Part 1: ", product(quadrant)
    
    do t = 1, 10000
        image = 0
        do i = 1, num_of_robots
            positions(i, :) = modulo(positions(i, :) + velocities(i, :), box_size)
            image(positions(i, 1), positions(i, 2)) = image(positions(i, 1), positions(i, 2)) + 1
        end do
        compactness = 0
        do idx = window, box_size(1) - window
            do idy = window, box_size(2) - window
                if (image(idx, idy) == 0) cycle
                compactness = compactness + image(idx, idy) * &
                    (image(idx, idy - 1) + image(idx, idy + 1) + image(idx - 1, idy) + image(idx + 1, idy))
            end do
        end do
        if (compactness > num_of_robots) then
            call display_image(image, box_size, t)
            exit
        end if
    end do
    
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
    
    subroutine display_image(imap, isize, iter)
        integer, intent(in) :: isize(2), iter
        integer, intent(in) :: imap(0 : isize(1) - 1, 0 : isize(2) - 1)
        character(len = isize(2)) :: str
        integer :: ix, iy
        write(*,*) "Part 2: ", iter
        do ix = 0, isize(1) - 1
        str = ""
            do iy = 1, isize(2)
                str(iy:iy)="_"
                if (imap(ix, iy - 1) > 0) then
                    str(iy:iy) = "#"
                end if
            end do
            write(*, '(A)') str
        end do
    end subroutine display_image
    
end program day_14