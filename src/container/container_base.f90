module svgf_container_base
    use svgf_element, only: svg_element_base, element_vector
    use svgf_shape
    implicit none
    private
    public :: svg_container_base

    type, abstract, extends(svg_element_base) :: svg_container_base
        type(element_vector) :: child
    contains
        procedure :: push_back
        procedure :: line => container_add_line
        procedure :: rect => container_add_rect
        procedure :: path => container_add_path
    end type

contains

    !> this is the general interface to add an element into container
    !> usually this method should be avoided
    !> create a shallow copy of pushed object
    !> it is recommended to use pointer as an actual argument
    subroutine push_back(this, element)
        class(svg_container_base), intent(inout) :: this
        class(svg_element_base), intent(in), target :: element
        class(svg_element_base), pointer :: ptr

        ptr => element
        call this%child%push_back(ptr)

    end subroutine push_back

    subroutine container_add_line(this, ptr, x1, y1, x2, y2, id)
        class(svg_container_base), intent(inout) :: this
        type(line_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: x1
        character(len=*), intent(in) :: y1
        character(len=*), intent(in) :: x2
        character(len=*), intent(in) :: y2
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_line_element(ptr, x1, y1, x2, y2, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine container_add_line

    subroutine container_add_rect(this, ptr, x, y, width, height, id)
        class(svg_container_base), intent(inout) :: this
        type(rect_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: x
        character(len=*), intent(in) :: y
        character(len=*), intent(in) :: width
        character(len=*), intent(in) :: height
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_rect_element(ptr, x, y, width, height, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine container_add_rect

    subroutine container_add_path(this, ptr, d, id)
        class(svg_container_base), intent(inout) :: this
        type(path_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: d
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_path_element(ptr, d, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine container_add_path

end module svgf_container_base
