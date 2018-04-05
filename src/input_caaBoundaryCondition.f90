subroutine input_caaBoundaryCondition
    use fson
    use fson_value_m, 	only: fson_value_count,fson_value_get    ! Functions for accessing data as an array
	use caaDataDefine,	only: westBoundaryAcousticPressure,&
							&eastBoundaryAcousticPressure,&
							&southBoundaryAcousticPressure,&
							&northBoundaryAcousticPressure,&
							&acousticPressureAmplitude,&
							&acousticSourceType,&
							sin_omega,gauss_T
	implicit none
	
	integer				::	i,cellNumber
	character(len=128)	::	nameOfBoundary,boundaryType,cellPosition

    ! Declare a pointer variables.  Always use a pointer with fson_value.
    type(fson_value), pointer :: json_data,array,item

    ! Parse the json file
    json_data => fson_parse("case.json")

	
	! Get the phone numbers as an array
    call fson_get(json_data, "boundaryConditions", array)
	
    print *, "================== Boundary Condition ==================="
    ! Loop through each array item
    do i = 1, fson_value_count(array)
		! Get the array item (this is an associative array)
		item => fson_value_get(array, i)
      
		! Lookup the values from the array
		call fson_get(item, "name", nameOfBoundary)
		call fson_get(item, "type", boundaryType)
      
		!设置对应的边界条件
		select case (trim(nameOfBoundary))
			case ('D')
				select case (trim(boundaryType))
					case ('softwall')
						southBoundaryAcousticPressure=0
					case default
						print *, "------------------ 不能识别的边界条件 -------------------"
						print *, "boundart name: ", trim(nameOfBoundary), ", type: ", trim(boundaryType)
						print *, "......"
						print *, "---------------------------------------------------------"
						print *, "  "
						print *, "========================================================="
						stop 
				end select
			case ('T')
				select case (trim(boundaryType))
					case ('softwall')
						northBoundaryAcousticPressure=0
					case default
						print *, "------------------ 不能识别的边界条件 -------------------"
						print *, "boundart name: ", trim(nameOfBoundary), ", type: ", trim(boundaryType)
						print *, "......"
						print *, "---------------------------------------------------------"
						print *, "  "
						print *, "========================================================="
						stop 
				end select
			case ('R')
				select case (trim(boundaryType))
					case ('softwall')
						eastBoundaryAcousticPressure=0
					case default
						print *, "------------------ 不能识别的边界条件 -------------------"
						print *, "boundart name: ", trim(nameOfBoundary), ", type: ", trim(boundaryType)
						print *, "......"
						print *, "---------------------------------------------------------"
						print *, "  "
						print *, "========================================================="
						stop 
				end select
			case ('L')
				select case (trim(boundaryType))
					case ('softwall')
						westBoundaryAcousticPressure=0
					case default
						print *, "------------------ 不能识别的边界条件 -------------------"
						print *, "boundart name: ", trim(nameOfBoundary), ", type: ", trim(boundaryType)
						print *, "......"
						print *, "---------------------------------------------------------"
						print *, "  "
						print *, "========================================================="
						stop 
				end select
		end select
		!边界条件设置结束
	  
		! Print out the values
		print *, "boundart name: ", trim(nameOfBoundary), "   | type: ", trim(boundaryType)
    end do
	print *, "========================================================="
	print *, " "
    print *, " "
	
	nullify(array)
	call fson_get(json_data, "acousticSource", array)
	
	print *, "============== Acoustic Sourse Condition ================"
	! Loop through each array item
    do i = 1, fson_value_count(array)
		! Get the array item (this is an associative array)
		item => fson_value_get(array, i)
      
		! Lookup the values from the array
		call fson_get(item, "position", cellPosition)
		call fson_get(item, "cellNumber", cellNumber)
		call fson_get(item, "type", acousticSourceType)
		call fson_get(item, "pressureAmplitude", acousticPressureAmplitude)
		if ( trim(acousticSourceType)== 'Sin' ) then
            call fson_get(item, "omega", sin_omega)
        else if ( trim(acousticSourceType)== 'Gauss' ) then
		    call fson_get(item, "TT", gauss_T)
        end if
      
	  
		! Print out the values
		print *, "---------------------------------------------------------"
		print *, "source position    = ", trim(cellPosition)
		print *, "source cell number = ", cellNumber
		print *, "source type        = ", trim(acousticSourceType)
		print *, "pressure amplitude = ", acousticPressureAmplitude
        if ( trim(acousticSourceType)== 'Sin' ) then
		    print *, "sin omega          = ", sin_omega
		else if ( trim(acousticSourceType)== 'Gauss' ) then
            print *, "gauss T            = ", gauss_T
        end if
		print *, "---------------------------------------------------------"
		print *, " "
    end do
	print *, "========================================================="
	print *, " "
    print *, " "
	
    ! clean up
    call fson_destroy(json_data)
	


end subroutine input_caaBoundaryCondition