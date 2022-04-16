module svgf_container_text
    use svgf_element, only: svg_element_base
    use svgf_container_base, only: svg_container_base
    use svgf_shape, only: tspan_element
    use svgf_utils, only: string_buffer_t, optval
    use svgf_attribute, only: attribs_to_string
    implicit none
    private
    public :: text_element

    type, extends(svg_container_base) :: text_element
        character(len=:), allocatable :: x
        character(len=:), allocatable :: y
    contains
        procedure :: serialize => serialize_text
        procedure :: get_tag => get_tag_text
        procedure :: destroy => destroy_text
        procedure :: tspan => text_add_tspan
    end type

contains

    function serialize_text(this) result(string)
        class(text_element), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t(capacity=60)

        call buf%append('<text')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(' x="'//this%x//'"')
        call buf%append(' y="'//this%y//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append('>')

        do i = 1, this%child%get_len()
            call buf%append(this%child%lst(i)%ptr%serialize())
        end do

        call buf%append('</text>')

        string = buf%to_string()

    end function serialize_text

    function get_tag_text(this) result(tag)
        class(text_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "text"

    end function get_tag_text

    subroutine destroy_text(this)
        class(text_element), intent(inout) :: this

    end subroutine destroy_text

    subroutine text_add_tspan(this, ptr, content, x, y, absolute_pos, id)
        class(text_element), intent(inout) :: this
        type(tspan_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: content
        character(len=*), intent(in), optional :: x
        character(len=*), intent(in), optional :: y
        logical, intent(in), optional :: absolute_pos
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        if (associated(ptr)) ptr => null()
        allocate (ptr)
        ptr%content = content

        ! use relative position in default
        if (optval(absolute_pos, .false.)) then
            ! absolute position
            ptr%x = optval(x, "0")
            ptr%y = optval(y, "0")
            ptr%use_relative = .false.
        else
            ! x, y represent dx, dy
            ptr%dx = optval(x, "0")
            ptr%dy = optval(y, "0")
            ptr%use_relative = .true.
        end if

        if (present(id)) call ptr%set_id(id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine text_add_tspan

end module svgf_container_text
