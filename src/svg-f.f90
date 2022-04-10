module svgf
    use svgf_svg
    use svgf_element
    use svgf_shape
    use svgf_container
    use svgf_attribute
    use svgf_utils
    implicit none
    private
    public :: svg, create_svg
    public :: svg_element_base, element_vector
    public :: line_element, rect_element, path_element
    public :: g_element
    public :: svg_attribute, attribute_vector
    public :: f2s, d2s, i2s
    public :: optval, string_buffer_t, move_string_buffer
    public :: unitless
    public :: cm, mm, em, ex, px, inch, pc, pt, percent
    public :: deg, rad
    public :: operator(*)
end module svgf