module fortran_yaml_c_interface
  use iso_c_binding
  use fortran_yaml_c_types
  implicit none

  private

  public :: parse

  type, bind(c) :: type_node_c
    
    ! Node
    integer(c_int) :: T
    
    ! Scalar
    integer(c_int) :: string_len
    type(c_ptr) :: string = c_null_ptr
    
    ! Dictionary
    type(c_ptr) :: first_keyvaluepair = c_null_ptr
    integer(c_int) :: key_len
    type(c_ptr) :: key = c_null_ptr
    type(c_ptr) :: value = c_null_ptr
    type(c_ptr) :: next_keyvaluepair = c_null_ptr
    
    ! List
    type(c_ptr) :: first_listitem = c_null_ptr
    type(c_ptr) :: node = c_null_ptr
    type(c_ptr) :: next_listitem = c_null_ptr
    
  end type
  
  interface
    function LoadFile_c(filename, err_len, err) result(ptr) bind(C, name="LoadFile_c")
      use, intrinsic :: iso_c_binding
      character(c_char), intent(in) :: filename(*)
      integer(c_int) :: err_len
      type(c_ptr) :: err
      type(c_ptr) :: ptr
    end function

    subroutine DestroyNode(root) bind(C, name="DestroyNode")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: root
    end subroutine

    subroutine DestroyChar(err) bind(C, name="DestroyChar")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: err
    end subroutine
  end interface
  
contains
  
  function LoadFile(filename, err) result(root)
    character(*), intent(in) :: filename
    character(:), allocatable, intent(out) :: err
    type(c_ptr) :: root

    character(c_char), allocatable :: filename_c(:)
    character(c_char), pointer :: err_c(:)
    type(c_ptr) :: err_p
    integer(c_int) :: err_len

    ! prep the filename
    allocate(filename_c(len(filename)+1))
    call copy_string_ftoc(filename, filename_c)

    root = LoadFile_c(filename_c, err_len, err_p)

    if (c_associated(err_p)) then
      allocate(character(err_len)::err)
      call c_f_pointer(err_p, err_c, [err_len+1])
      call copy_string_ctof(err_c, err)
      call DestroyChar(err_p)
    endif
    
  end function
  
  recursive subroutine read_value(node_c_ptr, node)
    type(c_ptr), intent(in) :: node_c_ptr
    class(type_node), intent(inout), pointer :: node
    
    type(c_ptr) :: pair_c, item_c
    type(type_node_c), pointer :: node_c, pair, item
    character(c_char), pointer :: key(:)
    character(:), allocatable :: keyf
    character(c_char), pointer :: string(:)
    character(:), allocatable :: stringf
    
    class (type_node), pointer :: list_item
    class (type_node), pointer :: value
    
    call c_f_pointer(node_c_ptr, node_c)
    
    if (node_c%T == 1) then
      ! is map
      allocate(type_dictionary::node)      
      pair_c = node_c%first_keyvaluepair
      do while(c_associated(pair_c))
        call c_f_pointer(pair_c, pair)
        
        call c_f_pointer(pair%key, key, [pair%key_len+1]) ! buffer is len+1 to accomodate the terminating null char
        if (allocated(keyf)) deallocate(keyf)
        allocate(character(len=pair%key_len)::keyf)
        call copy_string_ctof(key, keyf)

        call read_value(pair%value, value)
        select type (node)
          class is (type_dictionary)
            call node%set(keyf, value)
        end select
        pair_c = pair%next_keyvaluepair
      enddo
    elseif (node_c%T == 2) then
      ! is sequence
      allocate(type_list::node)  
      item_c = node_c%first_listitem
      do while(c_associated(item_c))
        call c_f_pointer(item_c, item)
        call read_value(item%node, list_item)
        select type (node)
          class is (type_list)
            call node%append(list_item)
        end select
        item_c = item%next_listitem
      enddo
    elseif (node_c%T == 3) then
      ! is scalar
      allocate(type_scalar::node) 
      call c_f_pointer(node_c%string, string, [node_c%string_len+1]) ! buffer is len+1 to accomodate the terminating null char
      allocate(character(len=node_c%string_len)::stringf)
      call copy_string_ctof(string, stringf)
      select type (node)
        class is (type_scalar)
          node%string = stringf
      end select
    elseif (node_c%T == 4) then
      ! is null
      allocate(type_null::node) 
    else
      print*,"Something terrible broke in fortran-yaml-c. If this happens report a bug!"
      stop 1
    endif
    
  end subroutine  
  
  function parse(path, err) result(root)
    character(len=*), intent(in) :: path
    character(:), allocatable, intent(out) :: err
    class (type_node), pointer :: root
    type(c_ptr) :: root_c
    
    nullify(root)
    
    root_c = LoadFile(trim(path), err)
    if (c_associated(root_c)) then
      call read_value(root_c, root)
      call root%set_path("")
      call DestroyNode(root_c)
    endif

  end function

  subroutine copy_string_ctof(stringc,stringf)
    ! utility function to convert c string to fortran string
    character(len=*), intent(out) :: stringf
    character(c_char), intent(in) :: stringc(*)
    integer j
    stringf = ''
    char_loop: do j=1,len(stringf)
       if (stringc(j)==c_null_char) exit char_loop
       stringf(j:j) = stringc(j)
    end do char_loop
  end subroutine

  subroutine copy_string_ftoc(stringf, stringc)
    ! utility function to convert c string to fortran string
    character(len=*), intent(in) :: stringf
    character(c_char), intent(out) :: stringc(:)
    integer j, n, n1, n2
    n1 = len_trim(stringf)  
    n2 = size(stringc) - 1
    n = min(n1, n2)
    do j=1,n    
      stringc(j) = stringf(j:j)   
    end do
    stringc(n+1) = c_null_char
  end subroutine copy_string_ftoc

end module