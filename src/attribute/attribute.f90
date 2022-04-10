module svgf_attribute
    use svgf_attribute_base, only: svg_attribute
    use svgf_attribute_vector, only: attribute_vector
    use svgf_attribute_helper
    implicit none
    private
    public :: svg_attribute, attribute_vector
    public :: attribs_to_string, svg_viewbox


end module svgf_attribute
