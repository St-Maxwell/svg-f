program main
    use testdrive
    use iso_fortran_env
    use test_svgf_elements
    implicit none
    integer :: stat, is
    type(testsuite_type), dimension(:), allocatable :: testsuites
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("svgf_elements_tests", collect_svgf_elements_tests) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
    
end program main