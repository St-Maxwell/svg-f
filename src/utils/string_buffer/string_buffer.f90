module string_buffer
    implicit none
    private
    public :: string_buffer_t
    public :: move_string_buffer

    type :: string_buffer_t
        private
        character(len=:), allocatable :: buffer
        integer :: capacity = 0
        integer :: length = 0
    contains
        private
        procedure, pass, public :: get_capacity
        procedure, pass, public :: get_length
        procedure, pass, public :: append
        procedure, pass, public :: to_string
        procedure, pass, public :: clear
        procedure, pass :: ensure_capacity
        procedure, pass :: new_capacity
    end type

    interface string_buffer_t
        module procedure :: new_string_buffer
        module procedure :: new_string_buffer_with_capacity
        module procedure :: new_string_buffer_with_string
    end interface

contains

    function new_string_buffer() result(buf)
        type(string_buffer_t) :: buf

        buf%capacity = 16
        buf%length = 0
        allocate (character(len=buf%capacity) :: buf%buffer)

    end function new_string_buffer

    function new_string_buffer_with_capacity(capacity) result(buf)
        integer, intent(in) :: capacity
        type(string_buffer_t) :: buf

        buf%capacity = capacity
        buf%length = 0
        allocate (character(len=buf%capacity) :: buf%buffer)

    end function new_string_buffer_with_capacity

    function new_string_buffer_with_string(str) result(buf)
        character(len=*), intent(in) :: str
        type(string_buffer_t) :: buf

        buf%capacity = len(str) + 16
        buf%length = len(str)
        allocate (character(len=buf%capacity) :: buf%buffer)
        buf%buffer(:buf%length) = str(:)

    end function new_string_buffer_with_string

    function get_capacity(this) result(capacity)
        class(string_buffer_t), intent(in) :: this
        integer :: capacity

        capacity = this%capacity

    end function get_capacity

    function get_length(this) result(length)
        class(string_buffer_t), intent(in) :: this
        integer :: length

        length = this%length

    end function get_length

    subroutine append(this, str)
        class(string_buffer_t), intent(inout) :: this
        character(len=*), intent(in) :: str
        integer :: new_length

        new_length = this%length + len(str)
        call this%ensure_capacity(new_length)
        this%buffer(this%length + 1:new_length) = str(:)
        this%length = new_length

    end subroutine append

    subroutine ensure_capacity(this, minimum_capacity)
        class(string_buffer_t), intent(inout) :: this
        integer, intent(in) :: minimum_capacity
        character(len=:), allocatable :: tmp_buffer

        if (minimum_capacity < this%capacity) return

        this%capacity = this%new_capacity(minimum_capacity)
        allocate (character(len=this%capacity) :: tmp_buffer)
        tmp_buffer(:this%length) = this%buffer(:this%length)
        deallocate (this%buffer)
        call move_alloc(tmp_buffer, this%buffer)

    end subroutine ensure_capacity

    pure function new_capacity(this, minimum_capacity) result(capacity)
        class(string_buffer_t), intent(in) :: this
        integer, intent(in) :: minimum_capacity
        integer :: capacity

        capacity = this%capacity*2 + 2
        if (capacity < minimum_capacity) capacity = minimum_capacity
        if (capacity < 0 .and. minimum_capacity < 0) error stop

    end function new_capacity

    function to_string(this) result(str)
        class(string_buffer_t), intent(in) :: this
        character(len=:), allocatable :: str

        str = this%buffer(:this%length)

    end function to_string

    subroutine clear(this)
        class(string_buffer_t), intent(inout) :: this

        if (allocated(this%buffer)) deallocate (this%buffer)
        this%capacity = 16
        this%length = 0
        allocate (character(len=this%capacity) :: this%buffer)

    end subroutine clear

    subroutine move_string_buffer(from, to)
        type(string_buffer_t), intent(inout) :: from
        type(string_buffer_t), intent(inout) :: to

        call move_alloc(from%buffer, to%buffer)
        from%capacity = to%capacity
        from%length = to%length

        call to%clear()

    end subroutine move_string_buffer

end module string_buffer
