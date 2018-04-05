subroutine input_caaControl
    use fson
    use fson_value_m, 	only: fson_value_count,fson_value_get    ! Functions for accessing data as an array
	use caaDataDefine,	only: t_start,t_end,delta_t,particleVelocity,acousticPressure
	implicit none
	
	character(len=20)	::	material
	

    ! Declare a pointer variables.  Always use a pointer with fson_value.
    type(fson_value), pointer :: json_data

    ! Parse the json file
    json_data => fson_parse("case.json")

    ! Get the grids basical data
    ! call fson_get(json_data)
	call fson_get(json_data, "control.startTime", t_start)
    call fson_get(json_data, "control.endTime", t_end)
    call fson_get(json_data, "control.timeStep", delta_t)

	print *, "================== Control Parameters ==================="
	print *, "start time         = ", t_start
	print *, "end time           = ", t_end
	print *, "time step          = ", delta_t
	print *, "========================================================="
	print *, " "
    print *, " "
	
	call fson_get(json_data, "initialFile.particleVelocity", particleVelocity)
	call fson_get(json_data, "initialFile.acousticPressure", acousticPressure)
	
	print *, "================== Initial File Data =======+============"
	print *, "particle velocity  = ", particleVelocity
	print *, "acoustic pressure  = ", acousticPressure
	print *, "========================================================="
	print *, " "
    print *, " "
	
	! clean up
    call fson_destroy(json_data)


end subroutine input_caaControl