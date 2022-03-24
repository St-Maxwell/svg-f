module test_svgf_utils
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use svgf_utils
    implicit none
    private
    public :: collect_svgf_utils_tests

contains

!> =============================================================================

    subroutine collect_svgf_utils_tests(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("test_i2s", test_i2s), &
                    new_unittest("test_f2s", test_f2s), &
                    new_unittest("test_d2s", test_d2s), &
                    new_unittest("test_next_id", test_next_id) &
                    ]

    end subroutine collect_svgf_utils_tests

    subroutine test_i2s(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, i2s(-4), "-4")
        if (allocated(error)) return
        call check(error, i2s(129), "129")
        if (allocated(error)) return
        call check(error, i2s(0), "0")
        if (allocated(error)) return
        call check(error, i2s(-10000001), "-10000001")
        if (allocated(error)) return

    end subroutine test_i2s

    subroutine test_f2s(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, f2s(0.0), "0")
        if (allocated(error)) return
        call check(error, f2s(-0.0), "0")
        if (allocated(error)) return
        call check(error, f2s(100.0), "100")
        if (allocated(error)) return
        call check(error, f2s(100.01), "100.01")
        if (allocated(error)) return
        call check(error, f2s(-5.0), "-5")
        if (allocated(error)) return
        call check(error, f2s(1234.0), "1234")
        if (allocated(error)) return
        call check(error, f2s(2.1e12), "2.1E12")
        if (allocated(error)) return
        call check(error, f2s(2.0e12), "2.0E12")
        if (allocated(error)) return

    end subroutine test_f2s

    subroutine test_d2s(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, d2s(0.0_8), "0")
        if (allocated(error)) return
        call check(error, d2s(-0.0_8), "0")
        if (allocated(error)) return
        call check(error, d2s(100.0_8), "100")
        if (allocated(error)) return
        call check(error, d2s(100.01_8), "100.01")
        if (allocated(error)) return
        call check(error, d2s(-5.0_8), "-5")
        if (allocated(error)) return
        call check(error, d2s(1234.0_8), "1234")
        if (allocated(error)) return
        call check(error, d2s(2.1e12_8), "2.1E12")
        if (allocated(error)) return
        call check(error, d2s(2.0e12_8), "2.0E12")
        if (allocated(error)) return

    end subroutine test_d2s

    subroutine test_next_id(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, next_id(), "id0")
        if (allocated(error)) return
        call check(error, next_id(), "id1")
        if (allocated(error)) return
        call check(error, next_id(), "id2")
        if (allocated(error)) return
        call check(error, next_id(), "id3")
        if (allocated(error)) return
        call check(error, next_id(), "id4")
        if (allocated(error)) return

    end subroutine test_next_id

end module test_svgf_utils
