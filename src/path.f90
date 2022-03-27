module svgf_path
    use svgf_element, only: svg_element
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils
    use svgf_attribute_helper, only: attribs_to_string
    implicit none
    private
    public :: svg_path, new_path

    type, extends(svg_element) :: svg_path
        character(len=:), allocatable :: d
    contains
        procedure :: serialize => serialize_path
        procedure :: get_tag => get_tag_path
        procedure :: destroy => destroy_path
    end type

contains

    subroutine new_path(this, d, id)
        class(svg_element), pointer, intent(out) :: this
        character(len=*), intent(in) :: d
        character(len=*), intent(in), optional :: id

        allocate (svg_path :: this)

        select type (this)
        type is (svg_path)
            this%d = d
        end select

        if (present(id)) call this%set_id(id)

    end subroutine new_path

    function serialize_path(this) result(string)
        class(svg_path), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t(capacity=20)

        call buf%append('<path')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(' d="'//this%d//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append(' />')

        string = buf%to_string()

    end function serialize_path

    function get_tag_path(this) result(tag)
        class(svg_path), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "path"

    end function get_tag_path

    subroutine destroy_path(this)
        class(svg_path), intent(inout) :: this

        deallocate (this%d)

    end subroutine destroy_path

end module svgf_path
