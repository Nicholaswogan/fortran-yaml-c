
program test_yaml
  use, intrinsic :: iso_fortran_env, only:  output_unit
  use fortran_yaml_c, only: YamlFile
  implicit none
  call test1()
  call test2()
  call test3()
  call test4()
contains
  subroutine test1()
    type(YamlFile) :: file
    character(:), allocatable :: err

    call file%parse("../tests/test1.yaml", err)
    if (allocated(err)) then
      print*,err
      stop 1
    endif
    
    call file%dump(unit=output_unit, indent=0)
  end subroutine

  subroutine test2()
    type(YamlFile) :: file
    character(:), allocatable :: err

    call file%parse("../tests/test2.yaml", err)
    if (allocated(err)) then
      print*,err
      stop 1
    endif
    
    open(unit=2, file='test2_copy.yaml', form='formatted', status='replace')
    call file%dump(unit=2, indent=0)
    close(2)
  end subroutine

  subroutine test3()
    type(YamlFile) :: file
    character(:), allocatable :: err

    call file%parse("../tests/test3.yaml", err)
    if (.not.allocated(err)) then
      stop 1
    endif
    print*,err

  end subroutine

  subroutine test4()
    type(YamlFile) :: file
    character(:), allocatable :: err

    call file%parse("../tests/not_a_file.yaml", err)
    if (.not.allocated(err)) then
      stop 1
    endif
    print*,err

  end subroutine
end program


