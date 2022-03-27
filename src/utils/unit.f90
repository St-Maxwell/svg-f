module svgf_unit
    use svgf_utils, only: i2s, f2s
    implicit none
    private
    public :: cm, mm, em, ex, px, inch, pc, pt, percent
    public :: deg, rad
    public :: operator(*)

    type :: svg_unit
        character(len=3) :: unit
    end type

    !> public unit instance
    type(svg_unit), parameter :: cm = svg_unit("cm")
    type(svg_unit), parameter :: mm = svg_unit("mm")
    type(svg_unit), parameter :: em = svg_unit("em")
    type(svg_unit), parameter :: ex = svg_unit("ex")
    type(svg_unit), parameter :: px = svg_unit("px")
    type(svg_unit), parameter :: inch = svg_unit("in")
    type(svg_unit), parameter :: pc = svg_unit("pc")
    type(svg_unit), parameter :: pt = svg_unit("pt")
    type(svg_unit), parameter :: percent = svg_unit("%")
    type(svg_unit), parameter :: deg = svg_unit("deg")
    type(svg_unit), parameter :: rad = svg_unit("rad")

    interface operator(*)
        module procedure :: number_with_unit_int
        module procedure :: number_with_unit_real
    end interface

contains

    function number_with_unit_int(int, unit) result(string)
        integer, intent(in) :: int
        type(svg_unit), intent(in) :: unit
        character(len=:), allocatable :: string

        string = i2s(int)//trim(unit%unit)

    end function number_with_unit_int

    function number_with_unit_real(float, unit) result(string)
        real, intent(in) :: float
        type(svg_unit), intent(in) :: unit
        character(len=:), allocatable :: string

        string = f2s(float)//trim(unit%unit)

    end function number_with_unit_real

end module svgf_unit
