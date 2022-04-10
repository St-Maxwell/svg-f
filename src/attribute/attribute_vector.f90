module svgf_attribute_vector
    use svgf_attribute_base, only: svg_attribute
    implicit none
    private
    public :: attribute_vector

    integer, parameter :: initial_size = 16

    !> manager type for adding new attributes
    type :: attribute_vector
        type(svg_attribute), dimension(:), allocatable :: lst
        integer :: sz = 0
    contains
        procedure :: get_len
        procedure :: get_value
        procedure :: push_back_by_move
    end type

contains

    function get_len(this) result(length)
        class(attribute_vector), intent(in) :: this
        integer :: length

        length = this%sz

    end function get_len

    subroutine get_value(this, key, value)
        class(attribute_vector), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        integer :: i

        do i = 1, this%sz
            if (this%lst(i)%match_key(key)) then
                value = this%lst(i)%value
                return
            end if
        end do

        value = ""

    end subroutine get_value

    subroutine push_back_by_move(this, attr)
        class(attribute_vector), intent(inout) :: this
        type(svg_attribute), intent(inout) :: attr
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        call move_alloc(attr%key, this%lst(this%sz)%key)
        call move_alloc(attr%value, this%lst(this%sz)%value)

    end subroutine push_back_by_move

    subroutine resize(list, new_size)
        type(svg_attribute), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        type(svg_attribute), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            call move_alloc(list, tmp)
            allocate (list(new_size))

            do i = 1, min(size(tmp), new_size)
                if (allocated(tmp(i)%key)) then
                    call move_alloc(tmp(i)%key, list(i)%key)
                    call move_alloc(tmp(i)%value, list(i)%value)
                end if
            end do

            do i = new_size + 1, size(tmp)
                if (allocated(tmp(i)%key)) then
                    deallocate (tmp(i)%key)
                    deallocate (tmp(i)%value)
                end if
            end do

            deallocate (tmp)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

end module svgf_attribute_vector
