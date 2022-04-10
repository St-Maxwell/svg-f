module svgf_utils_i2s
    implicit none
    private
    public :: i2s

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

end module svgf_utils_i2s
