module test_path_action
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use svgf_shape_path_action
    implicit none
    private
    public :: collect_path_action_tests

contains

!> =============================================================================

    subroutine collect_path_action_tests(testsuite)
        type(unittest_type), dimension(:), allocatable, intent(out) :: testsuite

        testsuite = [ &
                    new_unittest("test_move", test_move), &
                    new_unittest("test_line", test_line), &
                    new_unittest("test_horizontal_line", test_horizontal_line), &
                    new_unittest("test_vertical_line", test_vertical_line), &
                    new_unittest("test_quadratic_curve", test_quadratic_curve), &
                    new_unittest("test_cubic_curve", test_cubic_curve), &
                    new_unittest("test_close", test_close), &
                    new_unittest("test_multiple_commands", test_multiple_commands) &
                    ]

    end subroutine collect_path_action_tests

    subroutine test_move(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%move_to("100", "30.0")
        call check(error, act%as_d(), "M100 30.0")
        call act%clear()
        call act%move_by("12.2", "30.0")
        call check(error, act%as_d(), "m12.2 30.0")
        if (allocated(error)) return

    end subroutine test_move

    subroutine test_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%line_to("100", "30.0")
        call check(error, act%as_d(), "L100 30.0")
        call act%clear()
        call act%line_by("12.2", "30.0")
        call check(error, act%as_d(), "l12.2 30.0")
        if (allocated(error)) return

    end subroutine test_line

    subroutine test_horizontal_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%horizontal_line_to("50")
        call check(error, act%as_d(), "H50")
        call act%clear()
        call act%horizontal_line_by("30.9")
        call check(error, act%as_d(), "h30.9")
        if (allocated(error)) return

    end subroutine test_horizontal_line

    subroutine test_vertical_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%vertical_line_to("50")
        call check(error, act%as_d(), "V50")
        call act%clear()
        call act%vertical_line_by("30.9")
        call check(error, act%as_d(), "v30.9")
        if (allocated(error)) return

    end subroutine test_vertical_line

    subroutine test_quadratic_curve(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%quadratic_curve_to("50", "20", "12.2", "21.1")
        call check(error, act%as_d(), "Q50 20, 12.2 21.1")
        call act%clear()
        call act%quadratic_curve_by("50", "20", "12.2", "21.1")
        call check(error, act%as_d(), "q50 20, 12.2 21.1")
        if (allocated(error)) return

    end subroutine test_quadratic_curve

    subroutine test_cubic_curve(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%cubic_curve_to("50", "20", "12.2", "21.1", "1", "1")
        call check(error, act%as_d(), "C50 20, 12.2 21.1, 1 1")
        call act%clear()
        call act%cubic_curve_by("50", "20", "12.2", "21.1", "1", "1")
        call check(error, act%as_d(), "c50 20, 12.2 21.1, 1 1")
        if (allocated(error)) return

    end subroutine test_cubic_curve

    subroutine test_close(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%close()
        call check(error, act%as_d(), "Z")
        if (allocated(error)) return

    end subroutine test_close


    subroutine test_multiple_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(path_action) :: act

        call act%move_to("100","100")
        call act%line_to("300","100")
        call act%line_to("200","300")
        call act%close()
        call check(error, act%as_d(), "M100 100 L300 100 L200 300 Z")
        if (allocated(error)) return

    end subroutine test_multiple_commands
    
end module test_path_action
