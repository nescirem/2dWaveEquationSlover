subroutine input_caaPhysicalParameters
    use fson
    use fson_value_m, 	only: fson_value_count,fson_value_get    ! Functions for accessing data as an array
	use caaDataDefine,	only: rho,soundSpeed
    implicit none

	integer				::	i,numOfMaterials=1
	character(len=128)	::	material

    ! Declare a pointer variables.  Always use a pointer with fson_value.
    type(fson_value), pointer :: json_data

    ! Parse the json file
    json_data => fson_parse("case.json")

    ! Get the grids basical data
    ! call fson_get(json_data)
	call fson_get(json_data, "physicalParameters.material", material)
    call fson_get(json_data, "physicalParameters.rho", rho)
    call fson_get(json_data, "physicalParameters.soundSpeed", soundSpeed)

	do i=1,numOfMaterials
		print *, "================== Physical Parameters =================="
		print *, "material            = ", trim(material)
		print *, "rho                 = ", rho
		print *, "soundSpeed          = ", soundSpeed
		print *, "========================================================="
		print *, " "
        print *, " "
	end do
	
	! clean up
    call fson_destroy(json_data)


end subroutine input_caaPhysicalParameters