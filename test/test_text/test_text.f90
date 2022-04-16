module test_text
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use svgf_container_text
    use svgf_shape_tspan
    implicit none
    private
    public :: collect_text_tests

contains

!> =============================================================================

    subroutine collect_text_tests(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("test_case1", test_case1), &
                    new_unittest("test_case2", test_case2), &
                    new_unittest("test_case3", test_case3), &
                    new_unittest("test_case3", test_case4), &
                    new_unittest("test_many_tspan", test_many_tspan) &
                    ]

    end subroutine collect_text_tests

    subroutine test_case1(error)
        type(error_type), allocatable, intent(out) :: error
        type(text_element) :: text
        type(tspan_element), pointer :: span

        text%x = "10"
        text%y = "20"
        call text%tspan(span, "first")

        call check(error, text%serialize(), '<text x="10" y="20"><tspan>first</tspan></text>')
        if (allocated(error)) return

    end subroutine test_case1

    subroutine test_case2(error)
        type(error_type), allocatable, intent(out) :: error
        type(text_element) :: text
        type(tspan_element), pointer :: span

        text%x = "0"
        text%y = "0"
        call text%tspan(span, "first", x="10", y="20")

        call check(error, text%serialize(), '<text x="0" y="0"><tspan dx="10" dy="20">first</tspan></text>')
        if (allocated(error)) return

    end subroutine test_case2

    subroutine test_case3(error)
        type(error_type), allocatable, intent(out) :: error
        type(text_element) :: text
        type(tspan_element), pointer :: span

        text%x = "0"
        text%y = "0"
        call text%tspan(span, "first", x="10", y="20", absolute_pos=.true.)

        call check(error, text%serialize(), '<text x="0" y="0"><tspan x="10" y="20">first</tspan></text>')
        if (allocated(error)) return

    end subroutine test_case3

    subroutine test_case4(error)
        type(error_type), allocatable, intent(out) :: error
        type(text_element) :: text
        type(tspan_element), pointer :: span

        text%x = "10"
        text%y = "20"
        call text%tspan(span, "first", absolute_pos=.true.)

        call check(error, text%serialize(), '<text x="10" y="20"><tspan x="0" y="0">first</tspan></text>')
        if (allocated(error)) return

    end subroutine test_case4

    subroutine test_many_tspan(error)
        type(error_type), allocatable, intent(out) :: error
        type(text_element) :: text
        type(tspan_element), pointer :: span

        text%x = "0"
        text%y = "0"
        call text%tspan(span, "first")
        call text%tspan(span, "second", x="0", y="5")
        call text%tspan(span, "third", x="0", y="10", absolute_pos=.true., id="last")
        call span%set_attrs("style","font-style:italic;")

        call check(error, text%serialize(), '<text x="0" y="0"><tspan>first</tspan>'// &
                                            '<tspan dy="5">second</tspan>'// &
                                            '<tspan id="last" x="0" y="10" style="font-style:italic;">'// &
                                            'third</tspan></text>')
        if (allocated(error)) return

    end subroutine test_many_tspan

end module test_text
