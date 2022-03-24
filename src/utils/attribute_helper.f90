module svgf_attribute_helper
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils
    implicit none
    private
    public :: attribs_to_string
    public :: svg_viewbox

contains

    function attribs_to_string(attribs) result(string)
        type(svg_attribute_vector), intent(in) :: attribs
        character(len=:), allocatable :: string
        type(string_buffer_t) :: buf
        integer :: i

        buf = string_buffer_t()
        do i = 1, attribs%get_len()
            call buf%append(' ')
            call buf%append(attribs%lst(i)%key)
            call buf%append('="')
            call buf%append(attribs%lst(i)%value)
            call buf%append('"')
        end do
        string = buf%to_string()

    end function attribs_to_string

    !> helper function returning viewBox attribute
    function svg_viewbox(x, y, width, height) result(viewbox)
        real, intent(in), optional :: x
        real, intent(in), optional :: y
        real, intent(in), optional :: width
        real, intent(in), optional :: height
        type(svg_attribute) :: viewbox
        real :: x_
        real :: y_
        real :: w_
        real :: h_

        x_ = optval(x, 0.)
        y_ = optval(y, 0.)
        w_ = optval(width, 0.)
        h_ = optval(height, 0.)

        viewbox%key = "viewBox"
        viewbox%value = f2s(x_) // ' ' // f2s(y_) // ' ' // f2s(w_) // ' ' // f2s(h_)

    end function svg_viewbox
    
end module svgf_attribute_helper