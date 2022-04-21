module svgf_shape_path_action
    use svgf_utils, only: string_buffer_t, f2s
    implicit none
    private
    public :: path_action

    integer, parameter :: initial_size = 16

    type :: path_command
        character(len=:), allocatable :: command
        !> coordinate parameters in path command are always unitless
        real, dimension(:), allocatable :: parameters
    end type

    type :: path_action
        type(path_command), dimension(:), allocatable :: cmds
        integer, private :: sz = 0
    contains
        procedure :: move_to
        procedure :: move_by
        procedure :: line_to
        procedure :: line_by
        procedure :: horizontal_line_to
        procedure :: horizontal_line_by
        procedure :: vertical_line_to
        procedure :: vertical_line_by
        procedure :: quadratic_curve_to
        procedure :: quadratic_curve_by
        procedure :: cubic_curve_to
        procedure :: cubic_curve_by
        procedure :: close
        procedure, private :: push_back => push_back_command
        procedure :: as_d
        procedure :: clear
    end type

contains

    subroutine move_to(this, x, y)
        class(path_action), intent(inout) :: this
        real, intent(in) :: x
        real, intent(in) :: y
        type(path_command) :: cmd

        cmd%command = 'M'
        cmd%parameters = [x, y]
        call this%push_back(cmd)

    end subroutine move_to

    subroutine move_by(this, dx, dy)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dx
        real, intent(in) :: dy
        type(path_command) :: cmd

        cmd%command = 'm'
        cmd%parameters = [dx, dy]
        call this%push_back(cmd)

    end subroutine move_by

    subroutine line_to(this, x, y)
        class(path_action), intent(inout) :: this
        real, intent(in) :: x
        real, intent(in) :: y
        type(path_command) :: cmd

        cmd%command = 'L'
        cmd%parameters = [x, y]
        call this%push_back(cmd)

    end subroutine line_to

    subroutine line_by(this, dx, dy)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dx
        real, intent(in) :: dy
        type(path_command) :: cmd

        cmd%command = 'l'
        cmd%parameters = [dx, dy]
        call this%push_back(cmd)

    end subroutine line_by

    subroutine horizontal_line_to(this, x)
        class(path_action), intent(inout) :: this
        real, intent(in) :: x
        type(path_command) :: cmd

        cmd%command = 'H'
        cmd%parameters = [x]
        call this%push_back(cmd)

    end subroutine horizontal_line_to

    subroutine horizontal_line_by(this, dx)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dx
        type(path_command) :: cmd

        cmd%command = 'h'
        cmd%parameters = [dx]
        call this%push_back(cmd)

    end subroutine horizontal_line_by

    subroutine vertical_line_to(this, y)
        class(path_action), intent(inout) :: this
        real, intent(in) :: y
        type(path_command) :: cmd

        cmd%command = 'V'
        cmd%parameters = [y]
        call this%push_back(cmd)

    end subroutine vertical_line_to

    subroutine vertical_line_by(this, dy)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dy
        type(path_command) :: cmd

        cmd%command = 'v'
        cmd%parameters = [dy]
        call this%push_back(cmd)

    end subroutine vertical_line_by

    subroutine quadratic_curve_to(this, x1, y1, x, y)
        class(path_action), intent(inout) :: this
        real, intent(in) :: x1
        real, intent(in) :: y1
        real, intent(in) :: x
        real, intent(in) :: y
        type(path_command) :: cmd

        cmd%command = 'Q'
        cmd%parameters = [x1, y1, x, y]
        call this%push_back(cmd)

    end subroutine quadratic_curve_to

    subroutine quadratic_curve_by(this, dx1, dy1, dx, dy)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dx1
        real, intent(in) :: dy1
        real, intent(in) :: dx
        real, intent(in) :: dy
        type(path_command) :: cmd

        cmd%command = 'q'
        cmd%parameters = [dx1, dy1, dx, dy]
        call this%push_back(cmd)

    end subroutine quadratic_curve_by

    subroutine cubic_curve_to(this, x1, y1, x2, y2, x, y)
        class(path_action), intent(inout) :: this
        real, intent(in) :: x1
        real, intent(in) :: y1
        real, intent(in) :: x2
        real, intent(in) :: y2
        real, intent(in) :: x
        real, intent(in) :: y
        type(path_command) :: cmd

        cmd%command = 'C'
        cmd%parameters = [x1, y1, x2, y2, x, y]
        call this%push_back(cmd)

    end subroutine cubic_curve_to

    subroutine cubic_curve_by(this, dx1, dy1, dx2, dy2, dx, dy)
        class(path_action), intent(inout) :: this
        real, intent(in) :: dx1
        real, intent(in) :: dy1
        real, intent(in) :: dx2
        real, intent(in) :: dy2
        real, intent(in) :: dx
        real, intent(in) :: dy
        type(path_command) :: cmd

        cmd%command = 'c'
        cmd%parameters = [dx1, dy1, dx2, dy2, dx, dy]
        call this%push_back(cmd)

    end subroutine cubic_curve_by

    subroutine close (this)
        class(path_action), intent(inout) :: this
        type(path_command) :: cmd

        cmd%command = 'Z'
        allocate (cmd%parameters(0))
        call this%push_back(cmd)

    end subroutine close

    function as_d(this) result(string)
        class(path_action), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i, j

        buf = string_buffer_t()
        do i = 1, this%sz
            if (i > 1) call buf%append(' ')
            call buf%append(this%cmds(i)%command)
            do j = 1, size(this%cmds(i)%parameters)
                if (j > 1) call buf%append(' ')
                call buf%append(f2s(this%cmds(i)%parameters(j)))
            end do
        end do

        string = buf%to_string()

    end function as_d

    subroutine push_back_command(this, command)
        class(path_action), intent(inout) :: this
        type(path_command), intent(inout) :: command
        integer :: sz

        if (.not. allocated(this%cmds)) then
            call resize(this%cmds, initial_size)
        end if

        sz = size(this%cmds)
        if (this%sz >= sz) then
            call resize(this%cmds, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        call move_alloc(command%command, this%cmds(this%sz)%command)
        call move_alloc(command%parameters, this%cmds(this%sz)%parameters)

    end subroutine push_back_command

    subroutine clear(this)
        class(path_action), intent(inout) :: this

        deallocate (this%cmds)
        this%sz = 0

    end subroutine clear

    subroutine resize(list, new_size)
        type(path_command), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        type(path_command), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            call move_alloc(list, tmp)
            allocate (list(new_size))

            do i = 1, min(size(tmp), new_size)
                if (allocated(tmp(i)%command)) then
                    call move_alloc(tmp(i)%command, list(i)%command)
                    call move_alloc(tmp(i)%parameters, list(i)%parameters)
                end if
            end do

            do i = new_size + 1, size(tmp)
                if (allocated(tmp(i)%command)) then
                    deallocate (tmp(i)%command)
                    deallocate (tmp(i)%parameters)
                end if
            end do

            deallocate (tmp)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

end module svgf_shape_path_action
