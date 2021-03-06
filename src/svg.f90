module svgf_svg
    use svgf_element
    use svgf_container
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: svg_document
    public :: create_svg_document

    type, extends(svg_container_base) :: svg_document
        character(len=:), allocatable :: width
        character(len=:), allocatable :: height
        character(len=26), private :: xmlns = "http://www.w3.org/2000/svg"
        character(len=3), private :: version = "1.1"
    contains
        procedure :: serialize => serialize_svg
        procedure :: get_tag => get_tag_svg
        procedure :: destroy => destroy_svg
        procedure :: g => svg_add_g
        procedure :: text => svg_add_text
    end type

contains

    subroutine create_svg_document(new_svg, width, height, id)
        type(svg_document), intent(inout) :: new_svg
        character(len=*), intent(in), optional :: width
        character(len=*), intent(in), optional :: height
        character(len=*), intent(in), optional :: id

        new_svg%width = optval(width, "100%")
        new_svg%height = optval(height, "100%")
        if (present(id)) call new_svg%set_id(id)

    end subroutine create_svg_document

    function serialize_svg(this) result(string)
        class(svg_document), intent(in) :: this
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t(capacity=60)

        call buf%append('<svg')
        if (allocated(this%id)) call buf%append(' id="'//this%id//'"')
        call buf%append(' width="'//this%width//'"')
        call buf%append(' height="'//this%height//'"')
        call buf%append(' xmlns="'//this%xmlns//'"')
        call buf%append(' version="'//this%version//'"')
        call buf%append(attribs_to_string(this%attrs))
        call buf%append('>')

        do i = 1, this%child%get_len()
            call buf%append(this%child%lst(i)%ptr%serialize())
        end do

        call buf%append('</svg>')

        string = buf%to_string()

    end function serialize_svg

    function get_tag_svg(this) result(tag)
        class(svg_document), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "svg"

    end function get_tag_svg

    subroutine destroy_svg(this)
        class(svg_document), intent(inout) :: this

    end subroutine destroy_svg

    subroutine svg_add_g(this, ptr, id)
        class(svg_document), intent(inout) :: this
        type(g_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_g_element(ptr, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine svg_add_g

    subroutine svg_add_text(this, ptr, x, y, id)
        class(svg_document), intent(inout) :: this
        type(text_element), pointer, intent(inout) :: ptr
        character(len=*), intent(in) :: x
        character(len=*), intent(in) :: y
        character(len=*), intent(in), optional :: id
        class(svg_element_base), pointer :: tmp

        call create_text_element(ptr, x, y, id)

        tmp => ptr
        call this%child%push_back(tmp)

    end subroutine svg_add_text

end module svgf_svg
