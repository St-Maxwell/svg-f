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
        integer :: line = 5
        integer :: rect = 6
        integer :: circle = 7
        integer :: ellipse = 8
        integer :: polyline = 9
        integer :: polygon = 10
        integer :: path = 11
    
    end type

    type(svgf_elements), parameter :: svg_tag = svgf_elements()

end module svgf_element_tags