module svgf_element
    use svgf_base, only: svg_base
    use svgf_attribute, only: svg_attribute, svg_attribute_vector
    use svgf_utils, only: next_id
    implicit none
    private
    public :: svg_element

    integer, parameter :: initial_size = 16

    type :: element_ptr
        class(svg_element), pointer :: ptr => null()
    contains
        final :: free_ptr
    end type

    type :: element_vector
        type(element_ptr), dimension(:), allocatable :: lst
        integer :: sz = 0
    contains
        procedure :: push_back
        procedure :: get_len
        procedure :: get_by_id
    end type

    type, extends(svg_base), abstract :: svg_element
        character(len=:), allocatable :: id
        type(svg_attribute_vector) :: attrs
        type(element_vector) :: child
        character(len=:), allocatable :: content
    contains
        !> serialize
        procedure(serialize), deferred :: serialize
        procedure(destroy), deferred :: destroy
        procedure :: add
        generic :: set_attrs => set_attrs_scalar, set_attrs_vector, set_attrs_array
        procedure, private :: set_attrs_scalar
        procedure, private :: set_attrs_vector
        procedure, private :: set_attrs_array
        procedure :: get_subelement_by_id
        procedure :: set_id
        procedure :: get_id
        procedure :: match_id
        procedure(get_tag), deferred :: get_tag
    end type

    abstract interface
        function serialize(this) result(string)
            import :: svg_element
            class(svg_element), intent(in) :: this
            character(len=:), allocatable :: string
        end function serialize

        subroutine destroy(this)
            import :: svg_element
            class(svg_element), intent(inout) :: this
        end subroutine destroy

        function get_tag(this) result(tag)
            import :: svg_element
            class(svg_element), intent(in) :: this
            character(len=:), allocatable :: tag
        end function get_tag
    end interface

contains

!>==========================================================

    subroutine push_back(this, element)
        class(element_vector), intent(inout) :: this
        class(svg_element), pointer, intent(inout) :: element
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
        class(svg_element), pointer, intent(out) :: ptr
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

!>==========================================================

    subroutine add(this, elem_ptr)
        class(svg_element), intent(inout) :: this
        class(svg_element), pointer, intent(inout) :: elem_ptr

        call this%child%push_back(elem_ptr)

    end subroutine add

    subroutine set_attrs_scalar(this, attrs)
        class(svg_element), intent(inout) :: this
        type(svg_attribute), intent(in) :: attrs

        call this%attrs%push_back(attrs)

    end subroutine set_attrs_scalar

    subroutine set_attrs_vector(this, attrs)
        class(svg_element), intent(inout) :: this
        type(svg_attribute_vector), allocatable, intent(inout) :: attrs

        call this%attrs%merge(attrs)

    end subroutine set_attrs_vector

    subroutine set_attrs_array(this, attrs)
        class(svg_element), intent(inout) :: this
        type(svg_attribute), dimension(:), allocatable, intent(inout) :: attrs

        call this%attrs%merge(attrs)

    end subroutine set_attrs_array

    function get_subelement_by_id(this, id) result(ptr)
        class(svg_element), intent(in), target :: this
        character(len=*), intent(in) :: id
        class(svg_element), pointer :: ptr
        integer :: i

        ptr => null()
        do i = 1, this%child%get_len()
            if (this%child%lst(i)%ptr%match_id(id)) then
                ptr => this%child%lst(i)%ptr
                exit
            end if
        end do

    end function get_subelement_by_id

    function get_id(this) result(id)
        class(svg_element), intent(inout) :: this
        character(len=:), allocatable :: id

        if (.not. allocated(this%id)) this%id = next_id()
        id = this%id

    end function get_id

    subroutine set_id(this, id)
        class(svg_element), intent(inout) :: this
        character(len=*), intent(in), optional :: id

        if (present(id)) then
            this%id = id
        else
            this%id = next_id()
        end if

    end subroutine set_id

    function match_id(this, id) result(matched)
        class(svg_element), intent(in) :: this
        character(len=*), intent(in) :: id
        logical :: matched

        matched = this%id == id

    end function match_id

!>==========================================================

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

end module svgf_element
