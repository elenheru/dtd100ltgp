
program hello

    implicit none
    integer :: i,j, intdum,na
    integer :: lr, lsr!, filenumber
    character( LEN = 34) :: filename_united
    character( LEN = 2 ) :: filenumber
    character( LEN = 23) :: stringdummy
    character( LEN = 02) :: stringdummy2
    real(8) :: values(6)
    real(8), allocatable, dimension(:) :: ux, uy, uz, rx, ry, rz, exx, exy, eyy, ezz, exz, ezy

    if(command_argument_count().ne.1)then
        print*, 'Error, two digits are required as command-line arguments, stopping'
        stop
    endif

    call get_command_argument(1, filenumber)
    !filenumber = '15'
    filename_united = "PeratomicRxyzUxyz_st-" // filenumber // "-un-100.txt"

    open(154, file = filename_united)
    read(154,1540) stringdummy, lr, lsr
    print *, "Hello World!", stringdummy, lr, lsr

    allocate(ux(lsr))
    allocate(uy(lsr))
    allocate(uz(lsr))
    allocate(rx(lsr))
    allocate(ry(lsr))
    allocate(rz(lsr))
    allocate(exx(lsr))
    allocate(exy(lsr))
    allocate(eyy(lsr))
    allocate(ezz(lsr))
    allocate(exz(lsr))
    allocate(ezy(lsr))


    intdum = 0
    na = 0
    !print*, na, i,j, "!!!"
    do i=1,lr
        do j=1,lr
            na = na + 1
            read(154,4412) intdum, rx(na), ry(na), rz(na), ux(na), uy(na), uz(na)
            !print*, na, i,j, "===", intdum
            PRINT 4412, na, rx(na), ry(na), rz(na), ux(na), uy(na), uz(na)
            !PRINT 4412, na, rx(na), ry(na), rz(na), ux(na), uy(na), uz(na)
        enddo
        !EXIT
        read(154,'(A2)') stringdummy2
    enddo
    !PRINT 4412, INTDUM, VALUES
    !PRINT 4412, INTDUM, rx(1), ry(2), rz(3), ux(4), uy(5), uz(6)
    !PRINT 4412, -1, rx(1), ry(1), rz(1), ux(1), uy(1), uz(1)
    !PRINT 4412, -2, rx(2), ry(2), rz(2), ux(2), uy(2), uz(2)
    !PRINT 4412, -3, rx(3), ry(3), rz(3), ux(3), uy(3), uz(3)
    !PRINT 4412, na, rx(na), ry(na), rz(na), ux(na), uy(na), uz(na)
    close(154)

    deallocate(ux)
    deallocate(uy)
    deallocate(uz)
    deallocate(rx)
    deallocate(ry)
    deallocate(rz)
    deallocate(exx)
    deallocate(exy)
    deallocate(eyy)
    deallocate(ezz)
    deallocate(exz)
    deallocate(ezy)


1540 format(A,I7.1,1x,I7.1)
4412 format(I7.2,SP,6(1x,E13.5e3),1x)
4413 format(I7.2,SP,6(1x,E13.5e3), A)
end program

