module svgf_utils_optval
    implicit none
    private
    public :: optval

    !> fallback function for optional dummy arguments
    interface optval
        module procedure :: optval_i
        module procedure :: optval_r
        module procedure :: optval_s
    end interface

contains

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

end module svgf_utils_optval
