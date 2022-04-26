module svgf_container
    use svgf_container_base, only: svg_container_base
    use svgf_container_g, only: g_element, create_g_element
    use svgf_container_text, only: text_element, create_text_element
    implicit none
    private
    public :: svg_container_base
    public :: g_element, create_g_element
    public :: text_element, create_text_element

end module svgf_container
