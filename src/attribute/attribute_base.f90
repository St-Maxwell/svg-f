module svgf_attribute_base
    implicit none
    private
    public :: svg_attribute

    !> SVG attributes, defined as key-value pairs
    type :: svg_attribute
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    contains
        procedure :: match_key
    end type

contains

    pure function match_key(this, key) result(matched)
        class(svg_attribute), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: matched

        matched = this%key == key

    end function match_key

end module svgf_attribute_base
