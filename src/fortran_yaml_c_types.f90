
module fortran_yaml_c_types
  use iso_fortran_env, only: dp => real64
  implicit none

  private

  public :: dp
  public :: type_node
  public :: type_dictionary, type_key_value_pair
  public :: type_list, type_list_item
  public :: type_scalar
  public :: type_null
  public :: type_error

  type,abstract :: type_node
    character(:), allocatable :: path
  contains
    procedure(node_dump), deferred :: dump
    procedure                      :: set_path => node_set_path
    procedure                      :: finalize => node_finalize
  end type

  abstract interface
    subroutine node_dump(self,unit,indent)
      import type_node
      class(type_node), intent(in) :: self
      integer,intent(in) :: unit, indent
    end subroutine
  end interface

  type,extends(type_node) :: type_scalar
    character(:), allocatable :: string
  contains
    procedure :: dump       => value_dump
    procedure :: to_logical => scalar_to_logical
    procedure :: to_integer => scalar_to_integer
    procedure :: to_real    => scalar_to_real
  end type

  type,extends(type_node) :: type_null
  contains
    procedure :: dump => null_dump
  end type

  type type_key_value_pair
    character(:), allocatable          :: key
    class (type_node),         pointer :: value => null()
    logical                            :: accessed = .false.
    type (type_key_value_pair),pointer :: next  => null()
  end type

  type,extends(type_node) :: type_dictionary
    type (type_key_value_pair),pointer :: first => null()
  contains
    procedure :: get            => dictionary_get
    procedure :: get_scalar     => dictionary_get_scalar
    procedure :: get_dictionary => dictionary_get_dictionary
    procedure :: get_list       => dictionary_get_list
    procedure :: get_string     => dictionary_get_string
    procedure :: get_logical    => dictionary_get_logical
    procedure :: get_integer    => dictionary_get_integer
    procedure :: get_real       => dictionary_get_real
    procedure :: set            => dictionary_set
    procedure :: set_string     => dictionary_set_string
    procedure :: dump           => dictionary_dump
    procedure :: flatten        => dictionary_flatten
    procedure :: reset_accessed => dictionary_reset_accessed
    procedure :: set_path       => dictionary_set_path
    procedure :: finalize       => dictionary_finalize
    procedure :: size           => dictionary_size
  end type

  type type_list_item
    class (type_node),    pointer :: node => null()
    type (type_list_item),pointer :: next => null()
  end type

  type,extends(type_node) :: type_list
    type (type_list_item),pointer :: first => null()
    type (type_list_item),pointer :: last  => null()
  contains
    procedure :: append   => list_append
    procedure :: dump     => list_dump
    procedure :: set_path => list_set_path
    procedure :: finalize => list_finalize
    procedure :: size     => list_size
  end type

  type type_error
    character(:), allocatable :: message
  end type

contains

  recursive subroutine node_finalize(self)
    class(type_node), intent(inout) :: self
  end subroutine

  subroutine dictionary_reset_accessed(self)
    class(type_dictionary), intent(in) :: self
    type(type_key_value_pair), pointer :: pair
    pair => self%first
    do while (associated(pair))
      pair%accessed = .false.
      pair => pair%next
    end do
  end subroutine

  function dictionary_get(self, key) result(value)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    class(type_node), pointer :: value

    type(type_key_value_pair), pointer :: pair

    nullify(value)
    pair => self%first
    do while (associated(pair))
      if (pair%key==key) exit
      pair => pair%next
    end do
    if (associated(pair)) then
      value => pair%value
      pair%accessed = .true.
    end if
  end function

  subroutine dictionary_set(self, key, value)
    class(type_dictionary), intent(inout) :: self
    character(len=*), intent(in) :: key
    class(type_node), pointer :: value

    type(type_key_value_pair), pointer :: pair

    if (.not.associated(self%first)) then
      ! This will be the first pair.
      allocate(self%first)
      pair => self%first
    else
      ! Try to find a pair with the same key, or failing that, the last pair.
      pair => self%first
      do while (associated(pair%next))
        if (pair%key==key) exit
        pair => pair%next
      end do
      if (.not.pair%key==key) then
        ! Key did not exist yet, which must mean we are operating on the last existing pair.
        ! Append a new pair.
        allocate(pair%next)
        pair => pair%next
      else
        deallocate(pair%value)
      end if
    end if

    ! Store key and value.
    pair%key = key
    pair%value => value
  end subroutine

  subroutine dictionary_set_string(self, key, value)
    class(type_dictionary), intent(inout) :: self
    character(len=*), intent(in) :: key, value

    class(type_scalar), pointer :: scalar_node
    class(type_node), pointer :: node

    allocate(scalar_node)
    scalar_node%string = value
    node => scalar_node
    call self%set(key, node)
  end subroutine

  subroutine value_dump(self, unit, indent)
    class (type_scalar),intent(in) :: self
    integer, intent(in) :: unit,indent
    write(unit,'(a)') trim(self%string)
  end subroutine

  subroutine null_dump(self,unit,indent)
    class(type_null), intent(in) :: self
    integer, intent(in) :: unit, indent
    write(unit,'(a)') 'null'
  end subroutine

  recursive subroutine dictionary_dump(self, unit, indent)
    class(type_dictionary), intent(in) :: self
    integer, intent(in) :: unit, indent
    type(type_key_value_pair), pointer :: pair

    logical :: first
    character(:), allocatable :: dump_key

    first = .true.
    pair => self%first
    do while (associated(pair))
      if (first) then
        first = .false.
      else
        write (unit,'(a)',advance='NO') repeat(' ',indent)
      end if

      dump_key = make_key_for_dump(pair%key)

      select type (value=>pair%value)
      class is (type_dictionary)
        if (value%size() > 0) then
          write (unit,'(a)') dump_key//':'
          write (unit,'(a)',advance='NO') repeat(' ',indent+2)
          call value%dump(unit,indent+2)
        else
          ! empty dictionary
          write (unit,'(a)') dump_key//': {}'
        endif
      class is (type_list)
        if (value%size() > 0) then
          write (unit,'(a)') dump_key//':'
          write (unit,'(a)',advance='NO') repeat(' ',indent+2)
          call value%dump(unit,indent+2)
        else
          ! empty list
          write (unit,'(a)') dump_key//': []'
        endif
      class default
        write (unit,'(a)',advance='NO') dump_key//': '
        call value%dump(unit,indent+len(dump_key)+2)
      end select
      pair => pair%next
    end do
  end subroutine

  pure function make_key_for_dump(key) result(dump_key)
    character(*), intent(in) :: key
    character(:), allocatable :: dump_key

    logical :: contains_whitespace
    integer :: i

    contains_whitespace = .false.
    do i = 1,len(key)
      if (key(i:i) == ' ') then
        contains_whitespace = .true.
        exit
      endif
    enddo

    if (contains_whitespace) then
      dump_key = '"'//key//'"'
    else
      dump_key = key
    endif

  end function

  recursive subroutine dictionary_flatten(self, target, prefix)
    class(type_dictionary), intent(in) :: self
    type(type_dictionary), intent(inout) :: target
    character(len=*), intent(in) :: prefix

    type(type_key_value_pair), pointer :: pair

    pair => self%first
    do while (associated(pair))
      select type (value=>pair%value)
      class is (type_scalar)
        call target%set_string(prefix//trim(pair%key),value%string)
      class is (type_dictionary)
        call value%flatten(target,prefix=prefix//trim(pair%key)//'/')
      end select
      pair => pair%next
    end do
  end subroutine

  function scalar_to_logical(self, default, success) result(value)
    class (type_scalar), intent(in) :: self
    logical, intent(in) :: default
    logical, optional, intent(out) :: success
    logical :: value
    character(len=:), allocatable :: tmp_string
    integer slen, tlen
    integer, parameter :: clen = 20

    character(len=clen), parameter :: true_strings(*) = &
      ['true','True','TRUE', &
       'on  ','On  ','ON  ', &
       'y   ','Y   ','yes ','Yes ','YES ']
    character(len=clen), parameter :: false_strings(*) = &
      ["false","False",'FALSE', &
       'off  ','Off  ','OFF  ', &
       'n    ','N    ','no   ','No   ','NO   ']

    slen = len(self%string)
    tlen = max(clen,slen)
    allocate(character(len=tlen) :: tmp_string)
    tmp_string = self%string // repeat(' ', max(0, tlen-slen))

    value = default

    if (any(tmp_string == true_strings)) then
      value = .true.
      if (present(success)) success = .true.
    elseif (any(tmp_string == false_strings)) then
      value = .false.
      if (present(success)) success = .true.
    else
      if (present(success)) success = .false.
    endif

  end function

  function scalar_to_integer(self, default, success) result(value)
    class(type_scalar), intent(in) :: self
    integer, intent(in) :: default
    logical, optional, intent(out) :: success
    integer :: value

    integer :: ios

    value = default
    read(self%string,*,iostat=ios) value
    if (present(success)) success = (ios == 0)  .and. (index(trim(adjustl(self%string)), " ") == 0)
  end function

  function scalar_to_real(self, default, success) result(value)
    class(type_scalar), intent(in)  :: self
    real(dp), intent(in) :: default
    logical, optional, intent(out) :: success
    real(dp) :: value

    integer :: ios

    value = default
    read(self%string,*,iostat=ios) value
    if (present(success)) success = (ios == 0)  .and. (index(trim(adjustl(self%string)), " ") == 0)
  end function

  recursive subroutine node_set_path(self, path)
    class (type_node),intent(inout) :: self
    character(len=*), intent(in) :: path
    self%path = path
  end subroutine

  recursive subroutine dictionary_set_path(self, path)
    class(type_dictionary), intent(inout) :: self
    character(len=*), intent(in) :: path

    type (type_key_value_pair),pointer :: pair

    self%path = path
    pair => self%first
    do while (associated(pair))
      call pair%value%set_path(trim(self%path)//'/'//trim(pair%key))
      pair => pair%next
    end do
  end subroutine

  function dictionary_get_scalar(self, key, required, error) result(scalar)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, intent(in) :: required
    type(type_error), allocatable :: error
    class(type_scalar), pointer :: scalar

    class (type_node), pointer :: node

    nullify(scalar)
    node => self%get(key)
    if (required.and..not.associated(node)) then
      allocate(error)
      error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
    end if
    if (associated(node)) then
      select type (node)
      class is (type_scalar)
        scalar => node
      class is (type_null)
        allocate(error)
        error%message = trim(node%path)//' must be set to a scalar value, not to null.'
      class is (type_dictionary)
        allocate(error)
        error%message = trim(node%path)//' must be set to a scalar value, not to a dictionary.'
      class is (type_list)
        allocate(error)
        error%message = trim(node%path)//' must be set to a scalar value, not to a list.'
      end select
    end if
  end function

  function dictionary_get_dictionary(self, key, required, error) result(dictionary)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, intent(in) :: required
    type(type_error), allocatable :: error
    class(type_dictionary), pointer :: dictionary

    class(type_node), pointer :: node

    nullify(dictionary)
    node => self%get(key)
    if (required.and..not.associated(node)) then
      allocate(error)
      error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
    end if
    if (associated(node)) then
      select type (typed_node=>node)
      class is (type_null)
        allocate(dictionary)
        dictionary%path = node%path
      class is (type_dictionary)
        dictionary => typed_node
      class default
        allocate(error)
        error%message = trim(node%path)//' must be a dictionary.'
      end select
    end if
  end function

  function dictionary_get_list(self, key, required, error) result(list)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, intent(in) :: required
    type(type_error), allocatable :: error
    class(type_list), pointer :: list

    class(type_node), pointer :: node

    nullify(list)
    node => self%get(key)
    if (required.and..not.associated(node)) then
      allocate(error)
      error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
    end if
    if (associated(node)) then
      select type (typed_node=>node)
      class is (type_null)
        allocate(list)
      class is (type_list)
        list => typed_node
      class default
        allocate(error)
        error%message = trim(node%path)//' must be a list.'
      end select
    end if
  end function

  function dictionary_get_string(self, key, default, error) result(value)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    character(len=*), optional, intent(in) :: default
    type(type_error), allocatable :: error
    character(len=:), allocatable :: value

    class(type_scalar), pointer :: node

    if (present(default)) value = default
    node => self%get_scalar(key,.not.present(default),error)
    if (associated(node)) value = node%string
  end function

  function dictionary_get_logical(self, key, default, error) result(value)
    class (type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    logical, optional, intent(in) :: default
    type(type_error), allocatable :: error
    logical :: value

    class(type_scalar), pointer :: node
    logical :: success

    if (present(default)) value = default
    node => self%get_scalar(key,.not.present(default),error)
    if (associated(node)) then
      value = node%to_logical(value,success)
      if (.not.success) then
        allocate(error)
        error%message = trim(node%path)//' is set to "'//trim(node%string) &
                      //'", which cannot be interpreted as a Boolean value.'
      end if
    end if
  end function

  function dictionary_get_integer(self, key, default, error) result(value)
    class(type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    integer, optional, intent(in) :: default
    type(type_error), allocatable :: error
    integer :: value

    class(type_scalar), pointer :: node
    logical :: success

    if (present(default)) value = default
    node => self%get_scalar(key,.not.present(default),error)
    if (associated(node)) then
      value = node%to_integer(value,success)
      if (.not.success) then
        allocate(error)
        error%message = trim(node%path)//' is set to "'//trim(node%string)//'", which cannot be interpreted as an integer.'
      end if
    end if
  end function

  function dictionary_get_real(self, key, default, error) result(value)
    class (type_dictionary), intent(in) :: self
    character(len=*), intent(in) :: key
    real(dp), optional, intent(in) :: default
    type(type_error),allocatable :: error
    real(dp) :: value

    class(type_scalar), pointer :: node
    logical :: success

    if (present(default)) value = default
    node => self%get_scalar(key,.not.present(default),error)
    if (associated(node)) then
      value = node%to_real(value,success)
      if (.not.success) then
        allocate(error)
        error%message = trim(node%path)//' is set to "'//trim(node%string)//'", which cannot be interpreted as a real number.'
      end if
    end if
  end function

  recursive subroutine dictionary_finalize(self)
    class(type_dictionary), intent(inout) :: self

    type(type_key_value_pair), pointer :: pair, next

    pair => self%first
    do while (associated(pair))
      next => pair%next
      call pair%value%finalize()
      deallocate(pair%value)
      deallocate(pair)
      pair => next
    end do
    nullify(self%first)
  end subroutine
  
  function dictionary_size(self) result(n)
    class(type_dictionary), intent(in) :: self
    integer :: n
    
    type(type_key_value_pair), pointer :: pair
    
    n = 0
    pair => self%first
    do while(associated(pair))
      n = n + 1
      pair => pair%next
    enddo
    
  end function

  subroutine list_append(self, node)
    class(type_list), intent(inout) :: self
    class(type_node), target :: node

    if (.not.associated(self%first)) then
      ! This will be the first item.
      allocate(self%first)
      self%first%node => node
      self%last => self%first
    else
      ! Append on the end of the list.
      allocate(self%last%next)
      self%last%next%node => node
      self%last => self%last%next
    end if
  end subroutine

  recursive subroutine list_dump(self, unit, indent)
    class(type_list), intent(in) :: self
    integer, intent(in) :: unit, indent

    type(type_list_item), pointer :: item
    logical :: first

    first = .true.
    item => self%first
    do while (associated(item))
      if (first) then
        first = .false.
      else
        write (unit,'(a)',advance='NO') repeat(' ',indent)
      end if
      write (unit,'(a)',advance='NO') '- '

      select type (value => item%node)
      class is (type_dictionary)
        if (value%size() > 0) then
          call value%dump(unit,indent+2)
        else
          ! empty dictionary
          write (unit,'(a)') '{}'
        endif
      class is (type_list)
        if (value%size() > 0) then
          call value%dump(unit,indent+2)
        else
          ! empty list
          write (unit,'(a)') '[]'
        endif
      class default
        call value%dump(unit,indent+2)
      end select
      item => item%next
    end do
  end subroutine

  recursive subroutine list_set_path(self, path)
    class(type_list), intent(inout) :: self
    character(len=*), intent(in) :: path

    type(type_list_item), pointer :: item
    integer :: inode
    character(len=6) :: strindex

    self%path = path
    inode = 0
    item => self%first
    do while (associated(item))
      write (strindex,'(i0)') inode
      call item%node%set_path(trim(self%path)//'['//trim(strindex)//']')
      inode = inode + 1
      item => item%next
    end do
  end subroutine

  recursive subroutine list_finalize(self)
    class(type_list), intent(inout) :: self

    type(type_list_item), pointer :: item, next

    item => self%first
    do while (associated(item))
      next => item%next
      call item%node%finalize()
      deallocate(item%node)
      deallocate(item)
      item => next
    end do
    nullify(self%first)
  end subroutine
   
  function list_size(self) result(n)
    class(type_list), intent(in) :: self
    integer :: n
    
    type (type_list_item),pointer :: item
    
    n = 0
    item => self%first
    do while(associated(item))
      n = n + 1
      item => item%next
    enddo
    
  end function

end module
