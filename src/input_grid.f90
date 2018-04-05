subroutine input_grid
    use fson
    use fson_value_m, only: fson_value_count, fson_value_get    ! Functions for accessing data as an array
	use typedef, only: grid_info
    integer         ::  numOfGridFiles
	integer			::	i
	implicit none

    ! Declare a pointer variables.  Always use a pointer with fson_value.
    type(fson_value), pointer :: json_data

	numOfGridFiles=1
    ! Parse the json file
    json_data => fson_parse("case1.json")

    ! Get the grids basical data
    ! call fson_get(json_data)
    allocate(grid(numOfGridFiles))
    call fson_get(json_data, "grid.fileName", grid(1)%fileName)
    call fson_get(json_data, "grid.dimension", grid(1)%dimension)
	call fson_get(json_data, "grid.type", grid(1)%type)
	call fson_get(json_data, "grid.scale", grid(1)%scale)

	do i=1,numOfGridFiles
		print *, "==================== grid basic info ===================="
		print *, "grid file name = ", trim(grid(i)%fileName)
		print *, "grid dimension = ", grid(i)%dimension
		print *, "grid type      = ", trim(grid(i)%type)
		print *, "grid sacle     = ", grid(i)%scale
		print *, "========================================================="
		print *, " "
	end do

	!if grid file name end with ".msh" then
		call input_msh
	!else if grid file name end with ".cgns" then
		!call input_cgns
	!else
		!print err
		!stop
	!end if

end subroutine input_grid