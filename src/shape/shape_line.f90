module svgf_shape_line
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: line_element, create_line_element
    
    type, extends(svg_element_base) :: line_element
        character(len=:), allocatable :: x1
        character(len=:), allocatable :: y1
        character(len=:), allocatable :: x2
        character(len=:), allocatable :: y2
    contains
        procedure :: serialize => serialize_line
        procedure :: get_tag => get_tag_line
        procedure :: destroy => destroy_line
    end type

contains

    subroutine create_line_element(new_line_element, x1, y1, x2, y2, id)
        type(line_element), pointer, intent(inout) :: new_line_element
        character(len=*), intent(in) :: x1
        character(len=*), intent(in) :: y1
        character(len=*), intent(in) :: x2
        character(len=*), intent(in) :: y2
        character(len=*), intent(in), optional :: id

        if (associated(new_line_element)) new_line_element => null()
        allocate(new_line_element)
        new_line_element%x1 = x1
        new_line_element%y1 = y1
        new_line_element%x2 = x2
        new_line_element%y2 = y2
        if (present(id)) call new_line_element%set_id(id)

    end subroutine create_line_element

    function serialize_line(this) result(string)
        class(line_element), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf

        buf = string_buffer_t(capacity=20)

        call buf%append('<line')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(' x1="'//this%x1//'"')
        call buf%append(' y1="'//this%y1//'"')
        call buf%append(' x2="'//this%x2//'"')
        call buf%append(' y2="'//this%y2//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append(' />')

        string = buf%to_string()

    end function serialize_line

    function get_tag_line(this) result(tag)
        class(line_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "line"

    end function get_tag_line

    subroutine destroy_line(this)
        class(line_element), intent(inout) :: this

        deallocate (this%x1)
        deallocate (this%y1)
        deallocate (this%x2)
        deallocate (this%y2)

    end subroutine destroy_line

end module svgf_shape_line