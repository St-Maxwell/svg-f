module svgf_shape_path
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: path_element

    type, extends(svg_element_base) :: path_element
        character(len=:), allocatable :: d
    contains
        procedure :: serialize => serialize_path
        procedure :: get_tag => get_tag_path
        procedure :: destroy => destroy_path
    end type

contains

    function serialize_path(this) result(string)
        class(path_element), intent(in) :: this
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
        class(path_element), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "path"

    end function get_tag_path

    subroutine destroy_path(this)
        class(path_element), intent(inout) :: this

        deallocate (this%d)

    end subroutine destroy_path

end module svgf_shape_path
