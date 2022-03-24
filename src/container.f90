module svgf_container
    use svgf_element, only: svg_element
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils
    use svgf_attribute_helper, only: attribs_to_string, svg_viewbox
    implicit none
    private
    public :: svg_svg, new_svg

    type, extends(svg_element) :: svg_svg
        private
        character(len=:), allocatable :: width
        character(len=:), allocatable :: height
        character(len=26) :: xmlns = "http://www.w3.org/2000/svg"
        character(len=3) :: version = "1.1"
    contains
        procedure :: serialize
        procedure :: get_tag
        procedure :: destroy
    end type

    interface svg_svg
        module procedure :: new_svg_func
    end interface

contains

    subroutine new_svg(this, width, height, id, attrs)
        type(svg_svg), allocatable, intent(out) :: this
        real, intent(in), optional :: width
        real, intent(in), optional :: height
        character(len=*), intent(in), optional :: id
        type(svg_attribute_vector), allocatable, intent(inout), optional :: attrs

        allocate(this)

        if (present(width)) then
            this%width = f2s(width)
        else
            this%width = "100%"
        end if

        if (present(height)) then
            this%height = f2s(height)
        else
            this%height = "100%"
        end if

        if (present(id)) call this%set_id(id)
        if (present(attrs)) call this%set_attrs(attrs)

    end subroutine new_svg

    function new_svg_func(width, height, id, attrs) result(svg)
        real, intent(in), optional :: width
        real, intent(in), optional :: height
        character(len=*), intent(in), optional :: id
        type(svg_attribute_vector), allocatable, intent(inout), optional :: attrs
        type(svg_svg) :: svg

        if (present(width)) then
            svg%width = f2s(width)
        else
            svg%width = "100%"
        end if

        if (present(height)) then
            svg%height = f2s(height)
        else
            svg%height = "100%"
        end if

        if (present(id)) svg%id = id
        if (present(attrs)) call svg%set_attrs(attrs)

    end function new_svg_func

    function serialize(this) result(string)
        class(svg_svg), intent(in) :: this
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

    end function serialize

    function get_tag(this) result(tag)
        class(svg_svg), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "svg"

    end function get_tag

    subroutine destroy(this)
        class(svg_svg), intent(inout) :: this

        deallocate(this%content)
        deallocate(this%width)
        deallocate(this%height)

    end subroutine destroy

end module svgf_container
