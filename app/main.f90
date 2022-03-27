
program main
    use svgf
    implicit none
    integer :: u
    class(svg_element), pointer :: svg
    class(svg_element), pointer :: g
    class(svg_element), pointer :: path
    class(svg_element), pointer :: rect
    type(svg_attribute), dimension(:), allocatable :: attrs
    character(len=*), parameter :: path_text1 = &
        "M40.297852 44.544922, 41.147461 40.443359, 56.967774 40.443359, 56.264649 44.544922 Z"
    character(len=*), parameter :: path_text2 = &
        "m 39.389648,71.556641 5.654297,-27.011719 l 0.849609,-4.101563 l 0.908203,-4.423828 "// &
        "q 0.703125,-3.310547 1.435547,-4.77539 0.761718,-1.464844 2.43164,-2.402344 1.699219,"// &
        "-0.966797 4.59961,-0.966797 2.021484,0 5.859375,0.849609 l -0.966797,4.628907 q -2.695313,"// &
        "-0.703125 -4.511719,-0.703125 -1.552734,0 -2.373047,0.791015 -0.791015,0.761719 -1.376953,"// &
        "3.66211 l -0.703125,3.339843 l -0.84961,4.101563 l -5.654297,27.011719 Z"
    character(len=*), parameter :: path_arrow = &
        "M43 25 L61 25 L54 18 L51.88 20.12 L53.758 22 L43 22 Z"

    call new_g(g, id="svgf-logo")

    call new_path(path, path_text1)
    call path%set_attrs("fill", "#5d3a7e")
    call g%add_child(path)

    call new_path(path, path_text2)
    call path%set_attrs("fill", "#815aa8")
    call g%add_child(path)

    call new_path(path, path_arrow)
    call path%set_attrs("fill", "#815aa8")
    call g%add_child(path)

    call new_rect(rect, i2s(7), i2s(50), i2s(30), i2s(6))
    attrs = [svg_attribute("transform","rotate(-45 7, 50)"), svg_attribute("fill","#5d3a7e")]
    call rect%set_attrs(attrs)
    call g%add_child(rect)

    call new_rect(rect, i2s(7), i2s(44), i2s(30), i2s(6))
    attrs = [svg_attribute("transform","rotate(45 7, 50)"), svg_attribute("fill","#815aa8")]
    call rect%set_attrs(attrs)
    call g%add_child(rect)

    call new_rect(rect, i2s(63), i2s(50), i2s(30), i2s(6))
    attrs = [svg_attribute("transform","rotate(45 93, 50)"), svg_attribute("fill","#5d3a7e")]
    call rect%set_attrs(attrs)
    call g%add_child(rect)

    call new_rect(rect, i2s(63), i2s(44), i2s(30), i2s(6))
    attrs = [svg_attribute("transform","rotate(-45 93, 50)"), svg_attribute("fill","#815aa8")]
    call rect%set_attrs(attrs)
    call g%add_child(rect)

    call new_svg(svg)
    call svg%set_attrs("viewBox", "7 18 86 54")
    call svg%add_child(g)

    open (newunit=u, file="./assets/svg-f.svg", action="write")
    write (u, "(A)") svg%serialize()
    close (u)

end program main
