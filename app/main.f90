
program main
    use svgf
    implicit none
    integer :: u
    type(svg_document) :: logo
    type(g_element), pointer :: g_ptr
    type(path_element), pointer :: path_ptr
    type(rect_element), pointer :: rect_ptr
    type(path_action) :: text1, text2, arrow

    call create_svg(logo)
    call logo%set_attrs("viewBox", "7 18 86 54")

    call logo%g(g_ptr, id="svgf-logo")

    call create_path_text1()
    call create_path_text2()
    call create_path_arrow()

    call g_ptr%path(path_ptr, text1%as_d())
    call path_ptr%set_attrs("fill", "#5d3a7e")

    call g_ptr%path(path_ptr, text2%as_d())
    call path_ptr%set_attrs("fill", "#815aa8")

    call g_ptr%path(path_ptr, arrow%as_d())
    call path_ptr%set_attrs("fill", "#815aa8")

    call g_ptr%rect(rect_ptr, 7*unitless, 50*unitless, 30*unitless, 6*unitless)
    call rect_ptr%set_attrs("transform", "rotate(-45 7, 50)")
    call rect_ptr%set_attrs("fill", "#5d3a7e")

    call g_ptr%rect(rect_ptr, 7*unitless, 44*unitless, 30*unitless, 6*unitless)
    call rect_ptr%set_attrs("transform", "rotate(45 7, 50)")
    call rect_ptr%set_attrs("fill", "#815aa8")

    call g_ptr%rect(rect_ptr, 63*unitless, 50*unitless, 30*unitless, 6*unitless)
    call rect_ptr%set_attrs("transform", "rotate(45 93, 50)")
    call rect_ptr%set_attrs("fill", "#5d3a7e")

    call g_ptr%rect(rect_ptr, 63*unitless, 44*unitless, 30*unitless, 6*unitless)
    call rect_ptr%set_attrs("transform", "rotate(-45 93, 50)")
    call rect_ptr%set_attrs("fill", "#815aa8")

    open (newunit=u, file="./assets/svg-f.svg", action="write")
    write (u, "(A)") logo%serialize()
    close (u)

contains

    subroutine create_path_text1()
        call text1%move_to(40.297852, 44.544922)
        call text1%line_to(41.147461, 40.443359)
        call text1%line_to(56.967774, 40.443359)
        call text1%line_to(56.264649, 44.544922)
        call text1%close()
    end subroutine create_path_text1

    subroutine create_path_text2()
        call text2%move_to(39.389648, 71.556641)
        call text2%line_by(5.654297, -27.011719)
        call text2%line_by(0.849609, -4.101563)
        call text2%line_by(0.908203, -4.423828)
        call text2%quadratic_curve_by(0.703125, -3.310547, 1.435547, -4.77539)
        call text2%quadratic_curve_by(0.761718, -1.464844, 2.43164, -2.402344)
        call text2%quadratic_curve_by(1.699219, -0.966797, 4.59961, -0.966797)
        call text2%quadratic_curve_by(2.021484, 0., 5.859375, 0.849609)
        call text2%line_by(-0.966797, 4.628907)
        call text2%quadratic_curve_by(-2.695313, -0.703125, -4.511719, -0.703125)
        call text2%quadratic_curve_by(-1.552734, 0., -2.373047, 0.791015)
        call text2%quadratic_curve_by(-0.791015, 0.761719, -1.376953, 3.66211)
        call text2%line_by(-0.703125, 3.339843)
        call text2%line_by(-0.84961, 4.101563)
        call text2%line_by(-5.654297, 27.011719)
        call text2%close()
    end subroutine create_path_text2

    subroutine create_path_arrow()
        call arrow%move_to(43., 25.)
        call arrow%line_to(61., 25.)
        call arrow%line_to(54., 18.)
        call arrow%line_to(51.88, 20.12)
        call arrow%line_to(53.758, 22.)
        call arrow%line_to(43., 22.)
        call arrow%close()
    end subroutine create_path_arrow

end program main
