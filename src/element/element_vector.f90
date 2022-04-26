module svgf_element_vector
    use svgf_element_base, only: svg_element_base
    implicit none
    private
    public :: element_vector

    integer, parameter :: initial_size = 16

    !> wrapper type of svg_element_bases pointer
    type :: element_ptr
        class(svg_element_base), pointer :: ptr => null()
    contains
        final :: free_ptr
    end type

    !> manager type for adding new svg_element_base object
    type :: element_vector
        type(element_ptr), dimension(:), allocatable :: lst
        integer :: sz = 0
    contains
        procedure :: push_back
        procedure :: get_len
        procedure :: get_by_id
    end type

contains

    subroutine push_back(this, element)
        class(element_vector), intent(inout) :: this
        class(svg_element_base), pointer, intent(inout) :: element
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        this%lst(this%sz)%ptr => element
        element => null()

    end subroutine push_back

    function get_len(this) result(length)
        class(element_vector), intent(in) :: this
        integer :: length

        length = this%sz

    end function get_len

    subroutine get_by_id(this, id, ptr)
        class(element_vector), intent(inout), target :: this
        character(len=*), intent(in) :: id
        class(svg_element_base), pointer, intent(out) :: ptr
        integer :: i

        ptr => null()
        do i = 1, this%sz
            if (associated(this%lst(i)%ptr)) then
                if (this%lst(i)%ptr%match_id(id)) then
                    ptr => this%lst(i)%ptr
                    exit
                end if
            end if
        end do

    end subroutine get_by_id

    subroutine resize(list, new_size)
        type(element_ptr), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        type(element_ptr), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            ! move list to tmp
            allocate (tmp(size(list)))
            do i = 1, size(list)
                tmp(i)%ptr => list(i)%ptr
                list(i)%ptr => null()
            end do
            deallocate (list)

            ! reallocate
            allocate (list(new_size))
            do i = 1, min(size(tmp), new_size)
                list(i)%ptr => tmp(i)%ptr
                tmp(i)%ptr => null()
            end do

            ! free used resource
            do i = new_size + 1, size(tmp)
                if (associated(tmp(i)%ptr)) call tmp(i)%ptr%destroy()
            end do
            deallocate (tmp)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

    impure elemental subroutine free_ptr(this)
        type(element_ptr), intent(inout) :: this

        if (associated(this%ptr)) deallocate (this%ptr)

    end subroutine free_ptr

end module svgf_element_vector
