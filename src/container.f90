module svgf_container
    use svgf_element, only: svg_element
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils
    use svgf_unit
    use svgf_attribute_helper, only: attribs_to_string
    implicit none
    private
    public :: svg_svg, new_svg
    public :: svg_g, new_g

    type, extends(svg_element) :: svg_svg
        private
        character(len=:), allocatable :: width
        character(len=:), allocatable :: height
        character(len=26) :: xmlns = "http://www.w3.org/2000/svg"
        character(len=3) :: version = "1.1"
    contains
        procedure :: serialize => serialize_svg
        procedure :: get_tag => get_tag_svg
        procedure :: destroy => destroy_svg
    end type

!>==========================================================

    type, extends(svg_element) :: svg_g
    contains
        procedure :: serialize => serialize_g
        procedure :: get_tag => get_tag_g
        procedure :: destroy => destroy_g
    end type

contains

    subroutine new_svg(this, width, height, id)
        class(svg_element), pointer, intent(out) :: this
        character(len=*), intent(in), optional :: width
        character(len=*), intent(in), optional :: height
        character(len=*), intent(in), optional :: id

        allocate (svg_svg :: this)

        select type (this)
        type is (svg_svg)
            if (present(width)) then
                this%width = width
            else
                this%width = 100*percent
            end if

            if (present(height)) then
                this%height = height
            else
                this%height = 100*percent
            end if
        end select

        if (present(id)) call this%set_id(id)

    end subroutine new_svg

    function serialize_svg(this) result(string)
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

    end function serialize_svg

    function get_tag_svg(this) result(tag)
        class(svg_svg), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "svg"

    end function get_tag_svg

    subroutine destroy_svg(this)
        class(svg_svg), intent(inout) :: this

        deallocate (this%content)
        deallocate (this%width)
        deallocate (this%height)

    end subroutine destroy_svg

!>==========================================================

    subroutine new_g(this, id)
        class(svg_element), pointer, intent(out) :: this
        character(len=*), intent(in), optional :: id

        allocate (svg_g :: this)
        if (present(id)) call this%set_id(id)

    end subroutine new_g

    function serialize_g(this) result(string)
        class(svg_g), intent(in) :: this
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
        class(svg_g), intent(in) :: this
        character(len=:), allocatable :: tag

        tag = "g"

    end function get_tag_g

    subroutine destroy_g(this)
        class(svg_g), intent(inout) :: this

    end subroutine destroy_g

end module svgf_container
