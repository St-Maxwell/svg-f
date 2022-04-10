module svgf_utils
    use ryu, only: f2s, d2s
    use string_buffer, only: string_buffer_t, move_string_buffer
    use svgf_utils_i2s, only: i2s
    use svgf_utils_optval, only: optval
    use svgf_utils_unit
    implicit none

contains

    !> generate id for SVG elements
    function next_id() result(id)
        character(len=:), allocatable :: id
        integer, save :: next_id_ = 0

        id = "id"//i2s(next_id_)
        next_id_ = next_id_ + 1

    end function next_id

    function verified_id(id) result(valid)
        character(len=*), intent(in) :: id
        logical :: valid

        !> to-do
        valid = .true.

    end function verified_id


end module svgf_utils
