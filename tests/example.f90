
program example
  implicit none
  call main()
contains
  subroutine main()
    use fortran_yaml_c, only: YamlFile, dp, &
                              type_node, type_dictionary, type_error, &
                              type_list, type_list_item, type_scalar
    
    type(YamlFile), target :: file
    character(:), allocatable :: err
    
    class(type_node), pointer :: root
    class(type_dictionary), pointer :: dict
    class(type_list), pointer :: list
    class(type_list_item), pointer :: item
    type(type_error), allocatable :: io_err
    
    character(:), allocatable :: string
    real(dp) :: pi
    logical :: happy
    
    call file%parse("../tests/test1.yaml", err)
    if (allocated(err)) then
      print*,err
      stop 1
    endif

    root => file%root
    
    select type (root)
    class is (type_dictionary)
      print'(A,i3)','Number of key-value pairs in root dictionary = ',root%size()
      
      pi = root%get_real('pi',error=io_err)
      if (allocated(io_err)) then
        print*,io_err%message
        stop 1
      endif
      
      print*,'pi =',pi
      
      happy = root%get_logical('happy-today',error=io_err)
      if (allocated(io_err)) then
        print*,io_err%message
        stop 1
      endif
      print*,"happy: ",happy
      
      dict => root%get_dictionary('reaction',required=.true.,error=io_err)
      if (allocated(io_err)) then
        print*,io_err%message
        stop 1
      endif
      
      string = dict%get_string("equation",error = io_err)
      if (allocated(io_err)) then
        print*,io_err%message
        stop 1
      endif
      print*,"reaction equation = ",string
      deallocate(string)
      
      list => root%get_list('groceries',required=.true.,error=io_err)
      if (allocated(io_err)) then
        print*,io_err%message
        stop 1
      endif
      
      print'(1x,a,i3)',"Number of elements in Grocery list: ",list%size()
      print*,"Grocery list:"
      item => list%first
      do while(associated(item))
        select type (element => item%node)
        class is (type_scalar)
          print'(3x,A)',element%string
          item => item%next
        end select
      enddo
      
    end select

  end subroutine
end program


