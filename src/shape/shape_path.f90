module svgf_shape_path
    use svgf_element
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: path_element, create_path_element

    type, extends(svg_element_base) :: path_element
        character(len=:), allocatable :: d
    contains
        procedure :: serialize => serialize_path
        procedure :: get_tag => get_tag_path
        procedure :: destroy => destroy_path
    end type

contains

    subroutine create_path_element(path_ptr, d, id)
        type(path_element), pointer, intent(inout) :: path_ptr
        character(len=*), intent(in) :: d
        character(len=*), intent(in), optional :: id

        if (associated(path_ptr)) path_ptr => null()
        allocate (path_ptr)
        path_ptr%d = d
        if (present(id)) call path_ptr%set_id(id)
        
    end subroutine create_path_element

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
