module ryu_utils
    use iso_fortran_env, only: int32, int64
    implicit none
    private
    public :: FLOAT_MANTISSA_BITS, FLOAT_MANTISSA_MASK, FLOAT_EXPONENT_BITS
    public :: FLOAT_EXPONENT_MASK, FLOAT_EXPONENT_BIAS
    public :: DOUBLE_MANTISSA_BITS, DOUBLE_MANTISSA_MASK, DOUBLE_EXPONENT_BITS
    public :: DOUBLE_EXPONENT_MASK, DOUBLE_EXPONENT_BIAS
    public :: accept_lower_bound, accept_upper_bound

    integer(kind=int32), parameter :: FLOAT_MANTISSA_BITS = 23_int32
    integer(kind=int32), parameter :: FLOAT_MANTISSA_MASK = shiftl(1, FLOAT_MANTISSA_BITS) - 1
    integer(kind=int32), parameter :: FLOAT_EXPONENT_BITS = 8_int32
    integer(kind=int32), parameter :: FLOAT_EXPONENT_MASK = shiftl(1, FLOAT_EXPONENT_BITS) - 1
    integer(kind=int32), parameter :: FLOAT_EXPONENT_BIAS = 127_int32

    integer(kind=int32), parameter :: DOUBLE_MANTISSA_BITS = 52_int32
    integer(kind=int64), parameter :: DOUBLE_MANTISSA_MASK = shiftl(1_int64, DOUBLE_MANTISSA_BITS) - 1_int64
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_BITS = 11_int32
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_MASK = shiftl(1_int32, DOUBLE_EXPONENT_BITS) - 1_int32
    integer(kind=int32), parameter :: DOUBLE_EXPONENT_BIAS = shiftl(1_int32, DOUBLE_EXPONENT_BITS - 1_int32) - 1_int32

contains

    !! now we always round to even
    pure function accept_upper_bound(even) result(r)
        logical(kind=int32), intent(in) :: even
        logical(kind=int32) :: r

        r = even

    end function accept_upper_bound

    pure function accept_lower_bound(even) result(r)
        logical(kind=int32), intent(in) :: even
        logical(kind=int32) :: r

        r = even

    end function accept_lower_bound

end module ryu_utils
