!>##########################################################################<!
!>      Published with The GNU General Public License (GNU GPL or GPL)      <!
!>            Copyright (c) 2018, Nescirem <https://nesic.site/>            <!
!>##########################################################################<!

program main
    use typedef
    use caaDataDefine
    implicit none 
    
    real(kind=8)                ::  TimeRightNow=0.0
	real(kind=8)				::	delta_x,delta_y,a_p,a_w,a_e,a_s,a_n,a_p_old,a_p_elder,DD,FF	!临时
	real(kind=8)				::	tec_delta_t,tec_time
	integer                     ::  n_step=0
	integer						::	i_row,i_col,i_output=0,i_cell=0
	integer						::  values(8)
	character(len=8)			::	start_time,ending_time
    integer                     ::  hh_s,mm_s,ss_s,hh_u,mm_u,ss_u
	
    call time (now)     ! read CPU time now
    start_time = now
    read(start_time,"(I2,1x,I2,1x,I2)") hh_s,mm_s,ss_s
    open(999,form='formatted',status='unknown',file='case.json')
    !*************************************************************************
!   call input_grid						!读入网格
    !*************************************************************************

	!*************************************************************************
!	call grid_reconstructure			!网格重构
	!*************************************************************************
    delta_x=0.01
    delta_y=0.01
	allocate(westBoundary(100),eastBoundary(100),northBoundary(100)&
			&,southBoundary(100))
	do i_row=1,100
		do i_col=1,100
			i_cell=i_cell+1
			Cell(i_col,i_row)%n_cell=i_cell
			Cell(i_col,i_row)%x=0.01*i_col-0.005
			Cell(i_col,i_row)%y=0.01*i_row-0.005
		end do
	end do
	Cell_old%n_cell=Cell%n_cell
	Cell_old%x=Cell%x
	Cell_old%y=Cell%y
	Cell_elder%n_cell=Cell%n_cell
	Cell_elder%x=Cell%x
	Cell_elder%x=Cell%y
	
	!*************************************************************************
	call input_caaPhysicalParameters	!读入流体域物性参数
	!*************************************************************************
	
	!*************************************************************************
	call input_caaBoundaryCondition		!读入边界条件
	!*************************************************************************
	
	do i_row=1,100
		westBoundary(i_row)%acousticPressure=westBoundaryAcousticPressure
		eastBoundary(i_row)%acousticPressure=eastBoundaryAcousticPressure
		northBoundary(i_row)%acousticPressure=northBoundaryAcousticPressure
		southBoundary(i_row)%acousticPressure=southBoundaryAcousticPressure
	end do
	
	!*************************************************************************
	call input_caaControl				!读入计算时间、时间步长等、初始化信息
	!*************************************************************************
	
	!*************************************************************************
!	call caaInitialize					!流场初始化
	!*************************************************************************
!	Cell%acousticPresure=acousticPressure
!	Cell%particleVelocity=particleVelocity
	Cell_old%acousticPressure=acousticPressure
	Cell_old%particleVelocity=particleVelocity
	Cell_elder%acousticPressure=acousticPressure
	Cell_elder%particleVelocity=particleVelocity
	
	!*************************************************************************
	call input_caaOutputControl			!输出控制
	!*************************************************************************
	
	close(999)

    !*************************************************************************
    do while (.true.)   				!迭代求解 call caaSlover
    !*************************************************************************
		n_step=n_step+1
		TimeRightNow=TimeRightNow+delta_t
		
		!*********************************************************************
		select case (trim(acousticSourceType))		!声源给定
		!*********************************************************************
			case ("Sin")
				Cell_old(50,50)%acousticPressure=acousticPressureAmplitude&
				&*sin(sin_omega * (TimeRightNow - delta_t))
			case ("Gauss")
                if (TimeRightNow < 0.0004) then
				    Cell_old(50,50)%acousticPressure=acousticPressureAmplitude&
				    &*exp(-((TimeRightNow-t_start)**2/gauss_T**2))
                end if
			case default
				print *, "================== 不能识别的声源条件 ==================="
				print *, "acoustic source type ", trim(acousticSourceType)
				print *, "......"
				print *, "========================================================="
				print *, "  "
				stop 
		end select
		
		!内部节点部分开始
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=(delta_x*delta_y)/(delta_t)
		a_p=FF
		a_e=DD*delta_t
		a_w=DD*delta_t
		a_n=DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		
		do i_row=2,99
			do i_col=2,99
				Cell(i_col,i_row)%acousticPressure=(&
				&  a_e*Cell_old(i_col+1,i_row)%acousticPressure&
				& +a_w*Cell_old(i_col-1,i_row)%acousticPressure&
				& +a_n*Cell_old(i_col,i_row+1)%acousticPressure&
				& +a_s*Cell_old(i_col,i_row-1)%acousticPressure&
				& +a_p_old*Cell_old(i_col,i_row)%acousticPressure&
				& +a_p_elder*Cell_elder(i_col,i_row)%acousticPressure&
				&)/a_p
			end do
		end do
		!内部节点部分结束
		
		!边界L部分开始
		DD=((soundSpeed**2)*delta_y)/delta_x
		a_p=FF
		a_e=DD*delta_t
		a_w=2*DD*delta_t
		a_n=DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		
		do i_row=2,99
			Cell(1,i_row)%acousticPressure=(&
			&  a_e*Cell_old(1+1,i_row)%acousticPressure&
			& +a_w*westBoundary(i_row)%acousticPressure&
			& +a_n*Cell_old(1,i_row+1)%acousticPressure&
			& +a_s*Cell_old(1,i_row-1)%acousticPressure&
			& +a_p_old*Cell_old(1,i_row)%acousticPressure&
			& +a_p_elder*Cell_elder(1,i_row)%acousticPressure&
			&)/a_p
		end do
		!边界L部分结束
		
		!边界R部分开始
		DD=((soundSpeed**2)*delta_y)/delta_x
		a_p=FF
		a_e=2*DD*delta_t
		a_w=DD*delta_t
		a_n=DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		
		do i_row=2,99
			Cell(100,i_row)%acousticPressure=(&
			&  a_e*eastBoundary(i_row)%acousticPressure&
			& +a_w*Cell_old(100-1,i_row)%acousticPressure&
			& +a_n*Cell_old(100,i_row+1)%acousticPressure&
			& +a_s*Cell_old(100,i_row-1)%acousticPressure&
			& +a_p_old*Cell_old(100,i_row)%acousticPressure&
			& +a_p_elder*Cell_elder(100,i_row)%acousticPressure&
			&)/a_p
		end do
		!边界R部分结束
		
		!边界T部分开始
		DD=((soundSpeed**2)*delta_y)/delta_x
		a_p=FF
		a_e=DD*delta_t
		a_w=DD*delta_t
		a_n=2*DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		
		do i_col=2,99
			Cell(i_col,100)%acousticPressure=(&
			&  a_e*Cell_old(i_col+1,100)%acousticPressure&
			& +a_w*Cell_old(i_col-1,100)%acousticPressure&
			& +a_n*northBoundary(i_col)%acousticPressure&
			& +a_s*Cell_old(i_col,100-1)%acousticPressure&
			& +a_p_old*Cell_old(i_col,100)%acousticPressure&
			& +a_p_elder*Cell_elder(i_col,100)%acousticPressure&
			&)/a_p
		end do
		!边界T部分结束
		
		!边界B部分开始
		DD=((soundSpeed**2)*delta_y)/delta_x
		a_p=FF
		a_e=DD*delta_t
		a_w=DD*delta_t
		a_n=DD*delta_t
		a_s=2*DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		
		do i_col=2,99
			Cell(i_col,1)%acousticPressure=(&
			&  a_e*Cell_old(i_col+1,1)%acousticPressure&
			& +a_w*Cell_old(i_col-1,1)%acousticPressure&
			& +a_n*Cell_old(i_col,1+1)%acousticPressure&
			& +a_s*southBoundary(i_col)%acousticPressure&
			& +a_p_old*Cell_old(i_col,1)%acousticPressure&
			& +a_p_elder*Cell_elder(i_col,1)%acousticPressure&
			&)/a_p
		end do
		!边界B部分结束
		
		!左下角点开始
		a_p=FF
		a_e=DD*delta_t
		a_w=2*DD*delta_t
		a_n=DD*delta_t
		a_s=2*DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		Cell(1,1)%acousticPressure=(a_e*Cell_old(1+1,1)%acousticPressure&
								& +a_w*westBoundary(1)%acousticPressure&
								& +a_n*Cell_old(1,1+1)%acousticPressure&
								& +a_s*southBoundary(1)%acousticPressure&
								& +a_p_old*Cell_old(1,1)%acousticPressure&
								& +a_p_elder*Cell_elder(1,1)%acousticPressure&
								&)/a_p
		!左下角角点结束
		
		!左上角点开始
		a_p=FF
		a_e=DD*delta_t
		a_w=2*DD*delta_t
		a_n=2*DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		Cell(1,100)%acousticPressure=(a_e*Cell_old(1+1,100)%acousticPressure&
								& +a_w*westBoundary(100)%acousticPressure&
								& +a_n*northBoundary(1)%acousticPressure&
								& +a_s*Cell_old(1,100-1)%acousticPressure&
								& +a_p_old*Cell_old(1,100)%acousticPressure&
								& +a_p_elder*Cell_elder(1,100)%acousticPressure&
								&)/a_p
		!左上角角点结束
		
		!右上角点开始
		a_p=FF
		a_e=2*DD*delta_t
		a_w=DD*delta_t
		a_n=2*DD*delta_t
		a_s=DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		Cell(100,100)%acousticPressure=(a_e*eastBoundary(100)%acousticPressure&
								& +a_w*Cell_old(100-1,100)%acousticPressure&
								& +a_n*northBoundary(100)%acousticPressure&
								& +a_s*Cell_old(1,100-1)%acousticPressure&
								& +a_p_old*Cell_old(1,1)%acousticPressure&
								& +a_p_elder*Cell_elder(1,1)%acousticPressure&
								&)/a_p
		!右上角角点结束
		
		!右下角点开始
		a_p=FF
		a_e=2*DD*delta_t
		a_w=DD*delta_t
		a_n=DD*delta_t
		a_s=2*DD*delta_t
		a_p_old=2*FF-(a_e+a_w+a_n+a_s)
		a_p_elder=-FF
		Cell(100,1)%acousticPressure=(a_e*eastBoundary(100)%acousticPressure&
								& +a_w*Cell_old(100-1,1)%acousticPressure&
								& +a_n*Cell_old(100,1+1)%acousticPressure&
								& +a_s*southBoundary(100)%acousticPressure&
								& +a_p_old*Cell_old(100,1)%acousticPressure&
								& +a_p_elder*Cell_elder(100,1)%acousticPressure&
								&)/a_p
		!右下角角点结束
		
		!*************************************************************************
		open(777,file='caaUnsteadyTecplotOutput.dat') 		!输出
		!*************************************************************************
		tec_delta_t=(t_end-t_start)/outputTimes
	
		tec_time=i_output*tec_delta_t
		if ( TimeRightNow>=tec_time .and. (TimeRightNow-delta_t)<=tec_time ) then
            i_output=i_output+1
			write(777,*) ' VARIABLES = "x","y","p"'
			write(777,*) 'ZONE T = " ', TimeRightNow, ' ",  DATAPACKING=BLOCK,', 'SOLUTIONTIME = ', TimeRightNow,  &
				& ', ZONETYPE=FEQUADRILATERAL,N=', 10000, ',E=',9801
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
			
			do i_row=1,99
				do i_col=1,99
					WRITE(777,*) i_col+(i_row-1)*100,i_col+(i_row-1)*100+1,i_col+i_row*100+1,i_col+i_row*100
				end do
			end do
			
		end if
		
        !caculate time cost
        call time(now)
        read(now,"(I2,1x,I2,1x,I2)") hh,mm,ss
        if (hh == hh_s) then
            hh_u=0
            if (mm == mm_s) then
                mm_u=0
                ss_u=ss-ss_s
                if (ss_u >= 60) then
                    mm_u=mm_u+1
                    ss_u=ss_u-60
                end if
            else
                mm_u=mm-mm_s
                ss_u=(60-ss_s)+ss
                if (ss_u >= 60) then
                    mm_u=mm_u+1
                    ss_u=ss_u-60
                else if (ss_u <60 ) then
                    mm_u=mm_u-1
                end if
            end if
        else 
            hh_u=hh-hh_s
            mm_u=(60-mm_s)+mm
            ss_u=(60-ss_s)+ss
            if (ss_u >= 60) then
                mm_u=mm_u+1
                ss_u=ss_u-60
            end if
            if (mm_u >= 60) then
                hh_u=hh_u+1
                mm_u=mm_u-60
            end if
        end if
		
        print *, "---------------------------------------------------------"
		print *, "step: ",n_step
		write (*,"(' cost time:',I3.2,'h',I3.2,'m',I3.2,'s')") hh_u,mm_u,ss_u
        print *, "---------------------------------------------------------"
		print *, "  "
		if ( TimeRightNow >= t_end) then
			print *, "================== May have converged ==================="
			print *, "Total steps:				",n_step,"steps"
			write (*,"(' Total cost time:',I3.2,'h',I3.2,'m',I3.2,'s')") hh_u,mm_u,ss_u
			print *, "========================================================="
			print *, "  "
			stop 
		end if
		

        Cell_elder=Cell_old
        Cell_old=Cell
        do i_row=1,100
		    do i_col=1,100
			    i_cell=i_cell+1
			    Cell(i_col,i_row)%n_cell=i_cell
			    Cell(i_col,i_row)%x=0.01*i_col-0.005
			    Cell(i_col,i_row)%y=0.01*i_row-0.005
		    end do
	    end do
		Cell%acousticPressure=acousticPressure
		Cell%particleVelocity=particleVelocity
	
    enddo
	
	!*************************************************************************
	close(777)

end program main