module svgf_shape
    use svgf_shape_line, only: line_element
    use svgf_shape_rect, only: rect_element
    use svgf_shape_path, only: path_element
    implicit none
    private
    public :: line_element, rect_element, path_element

end module svgf_shape
