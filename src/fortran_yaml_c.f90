module fortran_yaml_c
  use iso_c_binding
  use fortran_yaml_c_types
  use fortran_yaml_c_interface
  implicit none
  
  private
  
  ! types
  public :: dp
  public :: type_node
  public :: type_dictionary, type_key_value_pair
  public :: type_list, type_list_item
  public :: type_scalar
  public :: type_null
  public :: type_error
  
  ! parser
  public :: parse

  ! File API
  public :: YamlFile

  type :: YamlFile
    class(type_node), pointer :: root => null()
  contains 
    procedure :: parse => YamlFile_parse
    procedure :: dump => YamlFile_dump
    final :: YamlFile_final
  end type
  
contains

  subroutine YamlFile_parse(self, path, err)
    class(YamlFile), intent(inout) :: self
    character(*), intent(in) :: path
    character(:), allocatable, intent(out) :: err
    self%root => parse(path, err)
  end subroutine

  subroutine YamlFile_dump(self, unit, indent)
    class(YamlFile), intent(inout) :: self
    integer, intent(in) :: unit, indent
    if (associated(self%root)) then
      call self%root%dump(unit, indent)
    endif
  end subroutine

  subroutine YamlFile_final(self)
    type(YamlFile), intent(inout) :: self
    if (associated(self%root)) then
      call self%root%finalize()
      deallocate(self%root)
      nullify(self%root)
    endif
  end subroutine
  
end module