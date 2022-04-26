module svgf_container_g
    use svgf_container_base, only: svg_container_base
    use svgf_container_text, only: text_element, create_text_element
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: g_element, create_g_element

    type, extends(svg_container_base) :: g_element
    contains
        procedure :: serialize => serialize_g
        procedure :: get_tag => get_tag_g
        procedure :: destroy => destroy_g
        procedure :: g => g_add_g
        procedure :: text => g_add_text
    end type

contains

    subroutine create_g_element(g_ptr, id)
        type(g_element), pointer, intent(inout) :: g_ptr
        character(len=*), intent(in), optional :: id

        if (associated(g_ptr)) g_ptr => null()
        allocate (g_ptr)
        if (present(id)) call g_ptr%set_id(id)

    end subroutine create_g_element

    function serialize_g(this) result(string)
        class(g_element), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t(capacity=60)

        call buf%append('<g')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append('>')

        do i = 1, this%child%get_len()
            call buf%append(this%child%lst(i)%ptr%serialize())
        end do

        call buf%append('</g>')

        string = buf%to_string()

    end function serialize_g

    function get_tag_g(this) result(tag)
        class(g_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "g"

    end function get_tag_g

    subroutine destroy_g(this)
        class(g_element), intent(inout) :: this

    end subroutine destroy_g

    subroutine g_add_g(this, ptr, id)
        class(g_element), intent(inout) :: this
        type(g_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_g_element(ptr, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine g_add_g

    subroutine g_add_text(this, ptr, x, y, id)
        class(g_element), intent(inout) :: this
        type(text_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: x
        character(len=*), intent(in) :: y
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_text_element(ptr, x, y, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine g_add_text

end module svgf_container_g
