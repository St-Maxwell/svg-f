module test_svgf_elements
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use svgf_element
    use svgf_container
    use svgf_shape
    use svgf_attribute
    use svgf_attribute_helper
    implicit none
    private
!    public :: collect_svgf_elements_tests

contains

!!> =============================================================================
!
!    subroutine collect_svgf_elements_tests(testsuite)
!        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite
!
!        testsuite = [ &
!                    new_unittest("test_svg", test_svg), &
!                    new_unittest("test_line", test_line) &
!                    ]
!
!    end subroutine collect_svgf_elements_tests
!
!    subroutine test_svg(error)
!        type(error_type), allocatable, intent(out) :: error
!        character(len=*), parameter :: expected_svg = &
!        '<svg id="first" width="100" height="50" xmlns="http://www.w3.org/2000/svg" ' // &
!        'version="1.1" viewBox="0 0 50 50"></svg>'
!        type(svg_svg), allocatable :: svg
!
!        call new_svg(svg, width=100., height=50., id="first")
!        call svg%set_attrs(svg_viewbox(0.,0.,50.,50.))
!
!        write(*,"('GENERATED: ',A)") svg%serialize()
!        write(*,"('EXPECTED: ',A)") expected_svg
!        call check(error, svg%serialize(), expected_svg)
!        if (allocated(error)) return
!
!    end subroutine test_svg
!
!    subroutine test_line(error)
!        type(error_type), allocatable, intent(out) :: error
!        character(len=*), parameter :: expected_svg = &
!        '<svg width="100%" height="100%" xmlns="http://www.w3.org/2000/svg" ' // &
!        'version="1.1"><line x1="0" y1="0" x2="50" y2="60" stroke-width="10" /></svg>'
!        type(svg_svg), allocatable :: svg
!        class(svg_element), pointer :: line
!
!        call new_svg(svg)
!        call new_line(line, [0.,0.], [50.,60.])
!        call line%set_attrs(svg_attribute("stroke-width", "10"))
!        call svg%add(line)
!
!        write(*,"('ALLOCATION STATUS OF LINE: ',g0)") associated(line)
!        write(*,"('GENERATED: ',A)") svg%serialize()
!        write(*,"('EXPECTED: ',A)") expected_svg
!        call check(error, svg%serialize(), expected_svg)
!        if (allocated(error)) return
!
!    end subroutine test_line

end module test_svgf_elements
