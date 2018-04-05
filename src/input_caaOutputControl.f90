subroutine input_caaOutputControl
    use fson
    use fson_value_m, 	only: fson_value_count,fson_value_get    ! Functions for accessing data as an array
	use caaDataDefine,	only: outputTimes
	implicit none
	
    ! Declare a pointer variables.  Always use a pointer with fson_value.
    type(fson_value), pointer :: json_data

    ! Parse the json file
    json_data => fson_parse("case.json")

    ! Get the grids basical data
    ! call fson_get(json_data)
	call fson_get(json_data, "outputControl.outputTimes", outputTimes)


	print *, "=================== output Control  ====================="
	print *, "output times       = ", outputTimes
	print *, "......"
	print *, "========================================================="
	print *, " "
    print *, " "
	
	! clean up
    call fson_destroy(json_data)


end subroutine input_caaOutputControl