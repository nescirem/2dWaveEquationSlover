!============================================================================
module typedef      !������������������
!============================================================================
	!************************************************************************
	type grid_info		!���������ļ�������Ϣ
	!************************************************************************
		character(len=1024) ::	fileName
		character(len=1024)	::	type
		integer				::	dimension
		real(kind=8)		::	scale
	end type grid_info
    !************************************************************************
    type node_data		!����ڵ���������
    !************************************************************************
		real(kind=8)		::	x,y,volume
		integer				::	nCell
		integer,allocatable	::	cellLst(:)
    end type node_data
!    !************************************************************************
!    type face_data      !������Ԫ��������
!    !************************************************************************
!    
!    end type
!    !************************************************************************
!    type cell_data      !������Ԫ��������
!    !************************************************************************
!
!    end type
    !************************************************************************
    type boundary_data	!�߽���������
    !************************************************************************
		real(kind=8)				::	x,y
		integer						::	cellLeft,cellRight,nNodes
		integer,allocatable			::	nodelist(:)
    end type boundary_data
     
    type(node_data),allocatable     ::  node(:)
!    type(face_data),allocatable     ::  face(:)
    type(boundary_data),allocatable ::  bface(:)
!    type(cell_data),allocatable     ::  cell(:)
     
	integer nCells,nFaces,nNodes,nBFaces
    character*8 :: now
    integer     :: hh,mm,ss
    end module