module svgf_shape
    use svgf_element, only: svg_element
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils
    use svgf_attribute_helper, only: attribs_to_string
    implicit none
    private
    public :: svg_line, new_line
    public :: svg_rect, new_rect

    type, extends(svg_element) :: svg_line
        character(len=:), allocatable :: x1
        character(len=:), allocatable :: y1
        character(len=:), allocatable :: x2
        character(len=:), allocatable :: y2
    contains
        procedure :: serialize => serialize_line
        procedure :: get_tag => get_tag_line
        procedure :: destroy => destroy_line
    end type

!>==========================================================

    type, extends(svg_element) :: svg_rect
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

    subroutine new_line(this, x1, y1, x2, y2, id)
        class(svg_element), pointer, intent(out) :: this
        character(len=*), intent(in) :: x1
        character(len=*), intent(in) :: y1
        character(len=*), intent(in) :: x2
        character(len=*), intent(in) :: y2
        character(len=*), intent(in), optional :: id

        allocate (svg_line :: this)

        select type (this)
        type is (svg_line)
            this%x1 = x1
            this%y1 = y1
            this%x2 = x2
            this%y2 = y2
        end select

        if (present(id)) call this%set_id(id)

    end subroutine new_line

    function serialize_line(this) result(string)
        class(svg_line), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

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
        class(svg_line), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "line"

    end function get_tag_line

    subroutine destroy_line(this)
        class(svg_line), intent(inout) :: this

        deallocate (this%x1)
        deallocate (this%y1)
        deallocate (this%x2)
        deallocate (this%y2)

    end subroutine destroy_line

!>==========================================================

    subroutine new_rect(this, x, y, width, height, id)
        class(svg_element), pointer, intent(out) :: this
        character(len=*), intent(in) :: x
        character(len=*), intent(in) :: y
        character(len=*), intent(in) :: width
        character(len=*), intent(in) :: height
        character(len=*), intent(in), optional :: id

        allocate (svg_rect :: this)

        select type (this)
        type is (svg_rect)
            this%x = x
            this%y = y
            this%width = width
            this%height = height
        end select

        if (present(id)) call this%set_id(id)

    end subroutine new_rect

    function serialize_rect(this) result(string)
        class(svg_rect), intent(in) :: this
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
        class(svg_rect), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "rect"

    end function get_tag_rect

    subroutine destroy_rect(this)
        class(svg_rect), intent(inout) :: this

        deallocate (this%x)
        deallocate (this%y)
        deallocate (this%width)
        deallocate (this%height)

    end subroutine destroy_rect

end module svgf_shape
