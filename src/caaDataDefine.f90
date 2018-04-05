!============================================================================
module caaDataDefine			!计算用网格数据结构 
!============================================================================
	
	!************************************************************************
	type caaCell				!node data
	!************************************************************************
		real(kind=8)		::	acousticVelocityPotential
		real(kind=8)		::	acousticPressure,particleVelocity
		real(kind=8)		::	x,y
		integer				::	n_cell
    end type caaCell

	!************************************************************************
	type caaBoundary			!boundary data
	!************************************************************************
		real(kind=8)		::	acousticVelocityPotential
		real(kind=8)		::	acousticPressure,particleVelocity
		real(kind=8)		::	x,y
		integer				::	n_boundary
    end type caaBoundary
	
    type(caaCell)		        ::  Cell(100,100),Cell_old(100,100),Cell_elder(100,100)  !+1s
	type(caaBoundary),pointer,dimension(:)		::  westBoundary,eastBoundary&
													&,northBoundary,SouthBoundary
	!>...
	

    integer                     ::  outputTimes
	real(kind=8) 	    		::  soundSpeed,rho
	real(kind=8)				::	westBoundaryAcousticPressure,eastBoundaryAcousticPressure,&
									&southBoundaryAcousticPressure,northBoundaryAcousticPressure
	real(kind=8)				::	acousticPressureAmplitude,sin_omega,gauss_T
	real(kind=8)                ::  t_start,t_end,delta_t
	real(kind=8)				::	particleVelocity,acousticPressure
	character(len=128)			::	acousticSourceType
end module
	