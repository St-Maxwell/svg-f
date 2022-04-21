module svgf_shape
    use svgf_shape_line, only: line_element, create_line_element
    use svgf_shape_rect, only: rect_element
    use svgf_shape_path, only: path_element
    use svgf_shape_tspan, only: tspan_element
    use svgf_shape_path_action, only: path_action
    implicit none
    private
    public :: line_element, create_line_element
    public :: rect_element, path_element
    public :: tspan_element
    public :: path_action

end module svgf_shape
