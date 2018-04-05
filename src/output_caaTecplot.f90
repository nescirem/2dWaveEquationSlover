subroutine output_caaTecplot (tec_outputTimes,TimeNow)
	use caaDataDefine
	implicit none
	
	integer							tec_outputTimes
	real							TimeNow
	integer						::	i,i_row,i_col,i_output
	real(kind=8)                ::  time,tec_delta_t
	
	tec_delta_t=(t_end-t_start)/tec_outputTimes
	
	do i_output=0,tec_outputTimes-1
		time=i_output*tec_delta_t
		if ( TimeNow>time .and. TimeNow+delta_t<time ) then
			write(777,*) ' VARIABLES = "x","y","p"'
			write(777,*) 'ZONE T = " ', TimeNow, ' ",  DATAPACKING=BLOCK,', 'SOLUTIONTIME = ', TimeNow,  &
			  & ', ZONETYPE=FEQUADRILATERAL,N=', 10201, ',E=',10000
			do i_row=1,100
				do i_col=1,100
					WRITE(777,*) (Cell(i_col,i_row)%x)
				end do
			end do
			do i_row=1,100
				do i_col=1,100
					WRITE(777,*) (Cell(i_col,i_row)%y)
				end do 
			end do
			do i_row=1,100
				do i_col=1,100
					WRITE(777,*) (Cell(i_col,i_row)%acousticPressure)
				end do
			end do
		end if
	end do
	return
	
end subroutine output_caaTecplot