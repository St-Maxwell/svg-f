module svgf_utils
    use ryu
    use string_buffer
    implicit none
    private
    public :: i2s, f2s, d2s
    public :: string_buffer_t, move_string_buffer
    public :: svg_len, next_id, verified_id, optval

    
    !> helper function for SVG <length>
    interface svg_len
        module procedure :: svg_len_r
        module procedure :: svg_len_i
        module procedure :: svg_len_s
    end interface

    !> fallback function for optional dummy arguments
    interface optval
        module procedure :: optval_i
        module procedure :: optval_r
        module procedure :: optval_s
    end interface

contains

    !> integer to string
    pure function i2s(int) result(str)
        integer, value :: int
        character(len=:), allocatable :: str
        character(len=range(int) + 2) :: buffer
        character(len=1), dimension(0:*), parameter :: digits = &
                                                       ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
        integer :: n
        logical :: has_sign
        integer :: pos

        has_sign = int < 0
        if (has_sign) int = -int

        pos = len(buffer)
        do
            if (int < 10) then
                buffer(pos:pos) = digits(int)
                exit
            end if

            n = mod(int, 10)
            buffer(pos:pos) = digits(n)
            pos = pos - 1
            int = int/10
        end do

        if (has_sign) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        str = buffer(pos:)

    end function i2s

    !> helper function for SVG <length>
    function svg_len_r(length, unit) result(str)
        real, intent(in) :: length
        character(len=*), intent(in), optional :: unit
        character(len=:), allocatable :: str

        if (present(unit)) then
            str = f2s(length) // unit
        else
            str = f2s(length)
        end if

    end function svg_len_r

    function svg_len_i(length, unit) result(str)
        integer, intent(in) :: length
        character(len=*), intent(in), optional :: unit
        character(len=:), allocatable :: str

        if (present(unit)) then
            str = i2s(length) // unit
        else
            str = i2s(length)
        end if

    end function svg_len_i

    function svg_len_s(length, unit) result(str)
        character(len=*), intent(in) :: length
        character(len=*), intent(in), optional :: unit
        character(len=:), allocatable :: str

        if (present(unit)) then
            str = length // unit
        else
            str = length
        end if

    end function svg_len_s

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

    !> fallback function for optional dummy arguments
    pure elemental function optval_i(i, default) result(result)
        integer, intent(in), optional :: i
        integer, intent(in) :: default
        integer :: result

        if (present(i)) then
            result = i
        else
            result = default
        end if

    end function optval_i

    pure elemental function optval_r(r, default) result(result)
        real, intent(in), optional :: r
        real, intent(in) :: default
        real :: result

        if (present(r)) then
            result = r
        else
            result = default
        end if

    end function optval_r

    pure function optval_s(s, default) result(result)
        character(len=*), intent(in), optional :: s
        character(len=*), intent(in) :: default
        character(len=:), allocatable :: result

        if (present(s)) then
            result = s
        else
            result = default
        end if
    end function optval_s

end module svgf_utils
