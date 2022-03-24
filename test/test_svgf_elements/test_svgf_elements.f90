module test_svgf_elements
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use svgf_container
    use svgf_attribute_helper
    implicit none
    private
    public :: collect_svgf_elements_tests

contains

!> =============================================================================

    subroutine collect_svgf_elements_tests(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("test_svg", test_svg) &
                    ]

    end subroutine collect_svgf_elements_tests

    subroutine test_svg(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: expected_svg = &
        '<svg id="first" width="100" height="50" xmlns="http://www.w3.org/2000/svg" ' // &
        'version="1.1" viewBox="0 0 50 50"></svg>'
        type(svg_svg), allocatable :: svg

        call new_svg(svg, width=100., height=50., id="first")
        call svg%set_attrs(svg_viewbox(0.,0.,50.,50.))

        write(*,"('GENERATED: ',A)") svg%serialize()
        write(*,"('EXPECTED: ',A)") expected_svg
        call check(error, svg%serialize(), expected_svg)
        if (allocated(error)) return

    end subroutine test_svg


end module test_svgf_elements
