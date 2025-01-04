program day24
    use, intrinsic :: iso_fortran_env
    implicit none
    integer :: io, stat, n_lines, w1, w2, w, i, num_of_wire, num_of_gate, i1, i2
    integer, dimension(0:64) :: idx, idy, idz
    integer(int64) :: start_count, stop_count, rate, res, input1, input2
    character(len=512) :: line, msg
    type type_wire
        character(len=3) :: id = ""
        integer :: val = -1
    end type type_wire
    type type_gate
        integer :: oper = -1
        integer :: in_id(2) = -1
        integer :: out_id = -1
    end type type_gate
    type(type_wire), dimension(512) :: wire
    type(type_gate), dimension(512) :: gate
    integer, dimension(:), allocatable :: faulty
    
    call system_clock(count = start_count, count_rate = rate)
    open(newunit = io, file = "input24.txt", status = "old", &
        action = "read", iostat = stat, iomsg = msg)
    
    num_of_wire = 0
    do
        read(io, "(A)", iostat = stat) line
        if (line == "") exit
        num_of_wire = num_of_wire + 1
        wire(num_of_wire)%id = line(1:3)
        read(line(5:), *) wire(num_of_wire)%val
    end do
    
    num_of_gate = 0
    do 
        read(io, "(A)", iostat = stat) line
        if (stat /= 0) exit
        num_of_gate = num_of_gate + 1
        do w1 = 1, num_of_wire
            if (wire(w1)%id == line(1:3)) exit
        end do
         if (w1 == num_of_wire + 1) then 
            num_of_wire = num_of_wire + 1
            wire(num_of_wire)%id = line(1:3)
        end if
        gate(num_of_gate)%in_id(1) = w1
        line = line(5:)
        select case (line(1:3))
            case ("AND") ; gate(num_of_gate)%oper = 1
            case ("XOR") ; gate(num_of_gate)%oper = 2
            case ("OR")  ; gate(num_of_gate)%oper = 3
        end select
        line = trim(adjustl(line(4:)))
        do w2 = 1, num_of_wire
            if (wire(w2)%id == line(1:3)) exit
        end do
        if (w2 == num_of_wire + 1) then
            num_of_wire = num_of_wire + 1
            wire(num_of_wire)%id = line(1:3)
        end if
        gate(num_of_gate)%in_id(2) = w2
        line = line(8:)
        do w = 1, num_of_wire
            if (wire(w)%id == line(1:3)) exit
        end do
        if (w == num_of_wire + 1) then 
            num_of_wire = num_of_wire + 1
            wire(num_of_wire)%id = line(1:3)
        end if
        gate(num_of_gate)%out_id = w
    end do
    close(io)
    
    idx = 0
    idy = 0
    idz = 0
    do w = 1, num_of_wire
        select case (wire(w)%id(1:1))
        case ("x")
            read(wire(w)%id(2:3),*) i
            idx(i) = w
        case ("y")
            read(wire(w)%id(2:3),*) i
            idy(i) = w
        case ("z")
            read(wire(w)%id(2:3),*) i
            idz(i) = w
          end select
    end do

    call run_device(num_of_gate, gate, num_of_wire, wire, idZ, res)
    write(*,*) "Part 1: ", res
 
    allocate(faulty(0:44), source = 0)
    do i1 = 0, 0
        do i2 = 0, 44
            call reset_device(num_of_wire,wire)
            wire(idX(0:44))%val = 0
            wire(idY(0:44))%val = 0
            wire(idX(i1))%val = 1
            wire(idY(i2))%val = 1
            call run_device(num_of_gate, gate, num_of_wire, wire, idZ, res)
            input1 = 2**i1
            input2 = 2**i2
            if (res /= input1 + input2) then 
              write(*,'(2i3,3i20)') i1, i2, input1, input2, res
              faulty(i2) = 1
            end if
        end do
    end do
    
    write(*,*) "Part 2: "
    call system_clock(count = stop_count)
    write(*,*) "Time: ", real(stop_count - start_count) / real(rate), " seconds"
    
contains
    
    subroutine run_device(ngate, gate, nwire, wire, idZ, res)
        integer,intent(in) :: nwire,ngate
        type(type_gate),intent(in) :: gate(ngate)
        type(type_wire),intent(inout) :: wire(nwire)
        integer, intent(in) :: idZ(0:45)
        integer(int64), intent(out) :: res
        integer :: nact, ig, iw1, iw2, i1, i2, ow, iw, iz
        res = 0
        do 
            nact = 0
            do ig = 1, ngate
                iw1 = gate(ig)%in_id(1)
                iw2 = gate(ig)%in_id(2)
                ow = gate(ig)%out_id
                i1 = wire(iw1)%val
                i2 = wire(iw2)%val
                if (i1 < 0 .or. i2 < 0) cycle
                if ( wire(gate(ig)%out_id)%val >= 0) cycle 
                ! Now, we just have to make the computation of the gate ... 
                select case (gate(ig)%oper) 
                    case (1); wire(ow)%val = iand(i1,i2)
                    case (2); wire(ow)%val = ieor(i1,i2)
                    case (3); wire(ow)%val =  ior(i1,i2)
                end select
                nact = nact + 1
            end do
            ! no more gates can be activated : get out
            if (nact == 0) exit
        enddo
        res = 0
        do iz = 0, 45
            iw = idZ(iz)
            if (iw == 0) cycle
            if (wire(iw)%val == 1) then
                res = ibset(res, iz)
            end if
        end do
    end subroutine run_device 
    
    subroutine reset_device(nwire, wire)
        integer, intent(in) :: nwire
        type(type_wire), intent(inout) :: wire(nwire)
        integer :: iw
        do iw = 1, nwire
            wire(iw)%val = -1
        end do
    end subroutine reset_device
    
end program day24