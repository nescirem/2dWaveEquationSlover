!>##########################################################################<!
!>      Published with The GNU General Public License (GNU GPL or GPL)      <!
!>            Copyright (c) 2018, Nescirem <https://nesic.site/>            <!
!>##########################################################################<!

program main
    use typedef
    use caaDataDefine
    implicit none 
    
    real(kind=8)                ::  TimeRightNow=0.0
	real(kind=8)				::	delta_x,delta_y,a_p,a_w,a_e,a_s,a_n,a_p_old,a_p_elder,DD,FF	!��ʱ
	real(kind=8)				::	tec_delta_t,tec_time
	integer                     ::  outputTimes,n_step=0
	integer						::	i_row,i_col,i_output=0,i_cell=0
	integer						::  values(8)
	character(len=128)			::	date,time,zone
	
!	DATE_AND_TIME(data,time,zone,values)

    open(999,form='formatted',status='unknown',file='case.json')
    !*************************************************************************
!   call input_grid						!��������
    !*************************************************************************

	!*************************************************************************
!	call grid_reconstructure			!�����ع�
	!*************************************************************************
    delta_x=0.01
    delta_y=0.01
	allocate(Cell(100,100),Cell_old(100,100),Cell_elder(100,100))
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
	call input_caaPhysicalParameters	!�������������Բ���
	!*************************************************************************
	
	!*************************************************************************
	call input_caaBoundaryCondition		!����߽�����
	!*************************************************************************
	
	do i_row=1,100
		westBoundary(i_row)%acousticPressure=westBoundaryAcousticPressure
		eastBoundary(i_row)%acousticPressure=eastBoundaryAcousticPressure
		northBoundary(i_row)%acousticPressure=northBoundaryAcousticPressure
		southBoundary(i_row)%acousticPressure=southBoundaryAcousticPressure
	end do
	
	!*************************************************************************
	call input_caaControl				!�������ʱ�䡢ʱ�䲽���ȡ���ʼ����Ϣ
	!*************************************************************************
	
	!*************************************************************************
!	call caaInitialize					!������ʼ��
	!*************************************************************************
!	Cell%acousticPresure=acousticPressure
!	Cell%particleVelocity=particleVelocity
	Cell_old%acousticPressure=acousticPressure
	Cell_old%particleVelocity=particleVelocity
	Cell_elder%acousticPressure=acousticPressure
	Cell_elder%particleVelocity=particleVelocity
	
	!*************************************************************************
!	call output_caaControl				!�������
	!*************************************************************************
    outputTimes=10
	
	close(999)

    !*************************************************************************
    do while (.true.)   				!������� call caaSlover
    !*************************************************************************
		n_step=n_step+1
		TimeRightNow=TimeRightNow+delta_t
		
		!*********************************************************************
		select case (trim(acousticSourceType))		!��Դ����
		!*********************************************************************
			case ("Sin")
				Cell_old(50,50)%acousticPressure=acousticPressureAmplitude&
				&*sin(sin_omega * (TimeRightNow - delta_t))
			case ("Gauss")
				Cell_old(50,50)%acousticPressure=acousticPressureAmplitude&
				&*exp(-((TimeRightNow-t_start)**2/gauss_T**2))
			case default
				print *, "================== ����ʶ�����Դ���� ==================="
				print *, "acoustic source type ", trim(acousticSourceType)
				print *, "......"
				print *, "========================================================="
				print *, "  "
				stop 
		end select
		
		!�ڲ��ڵ㲿�ֿ�ʼ
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=delta_x*delta_y/(delta_t**2)
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
		!�ڲ��ڵ㲿�ֽ���
		
		!�߽�L���ֿ�ʼ
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=delta_x*delta_y/(delta_t**2)
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
		!�߽�L���ֽ���
		
		!�߽�R���ֿ�ʼ
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=delta_x*delta_y/(delta_t**2)
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
		!�߽�R���ֽ���
		
		!�߽�T���ֿ�ʼ
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=delta_x*delta_y/(delta_t**2)
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
		!�߽�T���ֽ���
		
		!�߽�B���ֿ�ʼ
		DD=((soundSpeed**2)*delta_y)/delta_x
		FF=delta_x*delta_y/(delta_t**2)
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
		!�߽�B���ֽ���
		
		!���½ǵ㿪ʼ
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
		!���½ǽǵ����
		
		!���Ͻǵ㿪ʼ
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
		!���Ͻǽǵ����
		
		!���Ͻǵ㿪ʼ
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
		!���Ͻǽǵ����
		
		!���½ǵ㿪ʼ
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
		!���½ǽǵ����
		
		open(777,file='caaUnsteadyTecplotOutput.dat') 	
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
		
		print *, "step: ",n_step,",  Max pressure: "
		print *, "ʱ��������:  				","s"
		print *, "  "
		if ( TimeRightNow >= t_end) then
			print *, "================== May have converged ==================="
			print *, "Totol steps:				",n_step,"steps"
			print *, "ʱ��������:  				","s"
			print *, "========================================================="
			print *, "  "
			stop 
		end if
		
		Cell_elder 	=> 	Cell_old
		Cell_old	=>	Cell
		Cell		=>	Cell_elder
		Cell%acousticPressure=acousticPressure
		Cell%particleVelocity=particleVelocity
	
    enddo
	
	!*************************************************************************
	close(777)

end program main