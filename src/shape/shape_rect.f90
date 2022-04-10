module svgf_shape_rect
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: rect_element

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
