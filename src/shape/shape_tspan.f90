module svgf_shape_tspan
    use svgf_element, only: svg_element_base
    use svgf_utils, only: string_buffer_t
    use svgf_attribute, only: attribs_to_string
    implicit none
    private
    public :: tspan_element

    !> tspan element which is wrapped in text element
    !> it is not allowed to create an instance of tspan directly
    !> to create a tspan object, call text_obj%tspan()
    type, extends(svg_element_base) :: tspan_element
        character(len=:), allocatable :: content
        character(len=:), allocatable :: x
        character(len=:), allocatable :: y
        character(len=:), allocatable :: dx
        character(len=:), allocatable :: dy
        logical :: use_relative = .true.
    contains
        procedure :: serialize => serialize_tspan
        procedure :: get_tag => get_tag_tspan
        procedure :: destroy => destroy_tspan
    end type

contains

    function serialize_tspan(this) result(string)
        class(tspan_element), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf

        buf = string_buffer_t(capacity=20)

        call buf%append('<tspan')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')

        if (this%use_relative) then
            if (this%dx /= '0') call buf%append(' dx="'//this%dx//'"')
            if (this%dy /= '0') call buf%append(' dy="'//this%dy//'"')
        else
            call buf%append(' x="'//this%x//'"')
            call buf%append(' y="'//this%y//'"')
        end if

        call buf%append(attribs_to_string(this%attrs))
        call buf%append('>')

        ! text content
        if (allocated(this%content)) call buf%append(this%content)

        call buf%append('</tspan>')

        string = buf%to_string()

    end function serialize_tspan

    function get_tag_tspan(this) result(tag)
        class(tspan_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "tspan"

    end function get_tag_tspan

    subroutine destroy_tspan(this)
        class(tspan_element), intent(inout) :: this

        deallocate (this%content)
        deallocate (this%x)
        deallocate (this%y)
        deallocate (this%dx)
        deallocate (this%dy)

    end subroutine destroy_tspan

end module svgf_shape_tspan
