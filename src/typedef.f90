!============================================================================
module typedef      !定义数据类型与数据
!============================================================================
	!************************************************************************
	type grid_info		!定义网格文件基本信息
	!************************************************************************
		character(len=1024) ::	fileName
		character(len=1024)	::	type
		integer				::	dimension
		real(kind=8)		::	scale
	end type grid_info
    !************************************************************************
    type node_data		!定义节点数据类型
    !************************************************************************
		real(kind=8)		::	x,y,volume
		integer				::	nCell
		integer,allocatable	::	cellLst(:)
    end type node_data
!    !************************************************************************
!    type face_data      !定义面元数据类型
!    !************************************************************************
!    
!    end type
!    !************************************************************************
!    type cell_data      !定义体元数据类型
!    !************************************************************************
!
!    end type
    !************************************************************************
    type boundary_data	!边界数据类型
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