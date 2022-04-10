module svgf_element_base
    use svgf_attribute, only: svg_attribute, attribute_vector
    use svgf_utils
    implicit none
    private
    public :: svg_element_base

    type, abstract :: svg_element_base
        character(len=:), allocatable :: id
        type(attribute_vector) :: attrs
    contains
        !> serialize
        procedure(serialize), deferred :: serialize
        procedure(get_tag), deferred :: get_tag
        procedure(destroy), deferred :: destroy
        procedure :: set_attrs
        procedure :: set_id
        procedure :: get_id
        procedure :: match_id
    end type

    abstract interface
        function serialize(this) result(string)
            import :: svg_element_base
            class(svg_element_base), intent(in) :: this
            character(len=:), allocatable :: string
        end function serialize

        subroutine destroy(this)
            import :: svg_element_base
            class(svg_element_base), intent(inout) :: this
        end subroutine destroy

        function get_tag(this) result(tag)
            import :: svg_element_base
            class(svg_element_base), intent(in) :: this
            character(len=:), allocatable :: tag
        end function get_tag
    end interface

contains

    subroutine set_attrs(this, key, value)
        class(svg_element_base), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        type(svg_attribute) :: attrs

        attrs = svg_attribute(key, value)
        call this%attrs%push_back_by_move(attrs)

    end subroutine set_attrs

    subroutine set_id(this, id)
        class(svg_element_base), intent(inout) :: this
        character(len=*), intent(in), optional :: id

        if (present(id)) then
            this%id = id
        else
            this%id = next_id()
        end if

    end subroutine set_id

    function get_id(this) result(id)
        class(svg_element_base), intent(inout) :: this
        character(len=:), allocatable :: id

        if (.not. allocated(this%id)) this%id = next_id()
        id = this%id

    end function get_id

    function match_id(this, id) result(matched)
        class(svg_element_base), intent(in) :: this
        character(len=*), intent(in) :: id
        logical :: matched

        matched = this%id == id

    end function match_id

end module svgf_element_base
