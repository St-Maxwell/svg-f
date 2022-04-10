module svgf_container_base
    use svgf_element, only: svg_element_base, element_vector
    use svgf_shape
    implicit none
    private
    public :: svg_container_base

    type, abstract, extends(svg_element_base) :: svg_container_base
        type(element_vector) :: child
    contains
        procedure :: line => container_add_line
        procedure :: rect => container_add_rect
        procedure :: path => container_add_path
    end type

contains

    subroutine container_add_line(this, ptr, x1, y1, x2, y2, id)
        class(svg_container_base), intent(inout) :: this
        type(line_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: x1
        character(len=*), intent(in) :: y1
        character(len=*), intent(in) :: x2
        character(len=*), intent(in) :: y2
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        if (associated(ptr)) ptr => null()
        allocate (ptr)
        ptr%x1 = x1
        ptr%y1 = y1
        ptr%x2 = x2
        ptr%y2 = y2
        if (present(id)) call ptr%set_id(id)

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

        if (associated(ptr)) ptr => null()
        allocate (ptr)
        ptr%x = x
        ptr%y = y
        ptr%width = width
        ptr%height = height
        if (present(id)) call ptr%set_id(id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine container_add_rect

    subroutine container_add_path(this, ptr, d, id)
        class(svg_container_base), intent(inout) :: this
        type(path_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: d
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        if (associated(ptr)) ptr => null()
        allocate (ptr)
        ptr%d = d
        if (present(id)) call ptr%set_id(id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine container_add_path

end module svgf_container_base
