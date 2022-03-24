module svgf_attribute
    implicit none
    private
    public :: svg_attribute, svg_attribute_vector

    integer, parameter :: initial_size = 16

    type :: svg_attribute
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    contains
        procedure :: match_key
    end type

    type :: svg_attribute_vector
        type(svg_attribute), dimension(:), allocatable :: lst
        integer :: sz = 0
    contains
        procedure :: get_len
        procedure :: get_value
        procedure :: push_back
        generic :: merge => merge_array, merge_vector
        procedure, private :: merge_array
        procedure, private :: merge_vector
    end type

contains

    function get_len(this) result(length)
        class(svg_attribute_vector), intent(in) :: this
        integer :: length

        length = this%sz

    end function get_len

    subroutine get_value(this, key, value)
        class(svg_attribute_vector), intent(in) :: this
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

    subroutine push_back(this, attr)
        class(svg_attribute_vector), intent(inout) :: this
        type(svg_attribute), intent(in) :: attr
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        this%lst(this%sz)%key = attr%key
        this%lst(this%sz)%value = attr%value

    end subroutine push_back

    subroutine merge_array(this, array)
        class(svg_attribute_vector), intent(inout) :: this
        type(svg_attribute), dimension(:), allocatable, intent(inout) :: array
        integer :: sz, min_sz
        integer :: i

        if (.not. allocated(this%lst)) then
            call resize(this%lst, max(initial_size, size(array)))
        end if

        sz = size(this%lst)
        min_sz = this%sz + size(array)
        if (min_sz >= sz) then
            call resize(this%lst, min_sz + min_sz/2 + 1)
        end if

        do i = 1, size(array)
            call move_alloc(array(i)%key, this%lst(i + this%sz)%key)
            call move_alloc(array(i)%value, this%lst(i + this%sz)%value)
        end do
        this%sz = min_sz

        deallocate (array)

    end subroutine merge_array

    subroutine merge_vector(this, vector)
        class(svg_attribute_vector), intent(inout) :: this
        type(svg_attribute_vector), allocatable, intent(inout) :: vector
        integer :: sz, min_sz
        integer :: i

        if (.not. allocated(this%lst)) then
            call resize(this%lst, max(initial_size, vector%get_len()))
        end if

        sz = size(this%lst)
        min_sz = this%sz + vector%get_len()
        if (min_sz >= sz) then
            call resize(this%lst, min_sz + min_sz/2 + 1)
        end if

        do i = 1, vector%get_len()
            call move_alloc(vector%lst(i)%key, this%lst(i + this%sz)%key)
            call move_alloc(vector%lst(i)%value, this%lst(i + this%sz)%value)
        end do
        this%sz = min_sz

        deallocate (vector)

    end subroutine merge_vector

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

    pure function match_key(this, key) result(matched)
        class(svg_attribute), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: matched

        matched = this%key == key

    end function match_key

end module svgf_attribute
