module svgf_element_tags
    implicit none
    private
    public :: svg_tag

    type :: svgf_elements
    !> containers
        integer :: svg = 1
        integer :: g = 2
        integer :: defs = 3
        integer :: use = 4

    !> shapes
        integer :: line = 10
        integer :: rect = 11
        integer :: circle = 12
        integer :: ellipse = 13
        integer :: polyline = 14
        integer :: polygon = 15
        integer :: path = 16
    end type

    type(svgf_elements), parameter :: svg_tag = svgf_elements()

end module svgf_element_tags