
program test_yaml
  implicit none
  call main()
contains
  subroutine main()
    use, intrinsic :: iso_fortran_env, only:  output_unit
    use fortran_yaml_c, only: YamlFile
    
    type(YamlFile) :: file
    character(:), allocatable :: err
    
    call file%parse("../test.yaml", err)
    if (allocated(err)) then
      print*,trim(err)
      stop 1
    endif
    
    call file%dump(unit=output_unit,indent=0)
  end subroutine
end program


