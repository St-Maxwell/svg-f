module svgf_shape_rect
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: rect_element, create_rect_element

    type, extends(svg_element_base) :: rect_element
        character(len=:), allocatable :: x
        character(len=:), allocatable :: y
        character(len=:), allocatable :: width
        character(len=:), allocatable :: height
    contains
        procedure :: serialize => serialize_rect
        procedure :: get_tag => get_tag_rect
        procedure :: destroy => destroy_rect
    end type

contains

    subroutine create_rect_element(rect_ptr, x, y, width, height, id)
        type(rect_element), pointer, intent(inout) :: rect_ptr
        character(len=*), intent(in) :: x
        character(len=*), intent(in) :: y
        character(len=*), intent(in) :: width
        character(len=*), intent(in) :: height
        character(len=*), intent(in), optional :: id

        if (associated(rect_ptr)) rect_ptr => null()
        allocate (rect_ptr)
        rect_ptr%x = x
        rect_ptr%y = y
        rect_ptr%width = width
        rect_ptr%height = height
        if (present(id)) call rect_ptr%set_id(id)

    end subroutine create_rect_element

    function serialize_rect(this) result(string)
        class(rect_element), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t(capacity=20)

        call buf%append('<rect')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(' x="'//this%x//'"')
        call buf%append(' y="'//this%y//'"')
        call buf%append(' width="'//this%width//'"')
        call buf%append(' height="'//this%height//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append(' />')

        string = buf%to_string()

    end function serialize_rect

    function get_tag_rect(this) result(tag)
        class(rect_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "rect"

    end function get_tag_rect

    subroutine destroy_rect(this)
        class(rect_element), intent(inout) :: this

        deallocate (this%x)
        deallocate (this%y)
        deallocate (this%width)
        deallocate (this%height)

    end subroutine destroy_rect

end module svgf_shape_rect
