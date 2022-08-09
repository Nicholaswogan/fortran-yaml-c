
program test_yaml
  implicit none
  call main()
contains
  subroutine main()
    use, intrinsic :: iso_fortran_env, only:  output_unit
    use fortran_yaml_c, only: YamlFile, error_length
    
    type(YamlFile) :: file
    character(len=error_length) :: error
    
    call file%parse("../test.yaml", error)
    if (error/='') then
      print*,trim(error)
      stop 1
    endif
    
    call file%dump(unit=output_unit,indent=0)
  end subroutine
end program


