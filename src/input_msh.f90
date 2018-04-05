subroutine input_msh
    use typedef
    implicit none
    logical                     ::  alive,gridId
    integer                     ::  error
    character(len=80)           ::  temp,temp1,tempc0,tempc1
    character(len=80)           ::  currentString
    integer                     ::  faces3,faces4,faces5,faces6,faces8
    integer,external            ::  Hex2Dec
    integer                     i,j
        
    inquire(file=grid(1)%fileName,exist=alive)
    if(.not.alive)then
        write(*,*)trim(grid(1)%fileName)," doesn't exist."
        stop
    end if
             
    open(unit=gridId,file=grid(1)%fileName)
    do while(.true.)
        read(gridId,"(A80)",iostat=error)  currentString
            
        if(currentString(1:6)=="(10 (6")then
            nNodes=Hex2Dec(currentString(10:12),3)                      
            allocate(node(nNodes))
            do i=1,nNodes
                read(gridId,*,iostat=error) node(i)%x, node(i)%y  
            end do
        end if
            
        if(currentString(1:6)=="(13(0")then
            nfaces=Hex2Dec(currentString(9:11),3)
            nfaces=nfaces-80
        end if
            
        if(currentString(1:6)=="(13(3")then
            !temp=currentString(9:10)
            faces3=Hex2Dec(currentString(9:10),2)
            nbfaces=80
            allocate(bface(nbfaces))
            do i=1,faces3
                !read(gridId,*,iostat=error) bface(i)%nNodes,bface(i)%nodelist(1),bface(i)%nodelist(2),bface(i)%c0,bface(i)%c1
                read(gridId,*,iostat=error) bface(i)%nNodes,temp,temp1,tempc0,tempc1
                allocate(bface(i)%nodelist(bface(i)%nNodes))
                bface(i)%nodelist(1)=Hex2Dec(trim(temp),len(trim(temp)))
                bface(i)%nodelist(2)=Hex2Dec(trim(temp1),len(trim(temp1)))
                bface(i)%c0=Hex2Dec(trim(tempc0),len(trim(tempc0)))
                bface(i)%c1=Hex2Dec(trim(tempc1),len(trim(tempc1)))
            end do
        end if
            
        if(currentString(1:6)=="(13(4")then
            !temp=currentString(10:11)
            faces4=Hex2Dec(currentString(10:11),2)
            do i=faces3+1,faces4
                !read(gridId,*,iostat=error) bface(i)%nNodes,bface(i)%nodelist(1),bface(i)%nodelist(2),bface(i)%c0,bface(i)%c1
                read(gridId,*,iostat=error) bface(i)%nNodes,temp,temp1,tempc0,tempc1
                allocate(bface(i)%nodelist(bface(i)%nNodes))
                bface(i)%nodelist(1)=Hex2Dec(trim(temp),len(trim(temp)))
                bface(i)%nodelist(2)=Hex2Dec(trim(temp1),len(trim(temp1)))
                bface(i)%c0=Hex2Dec(trim(tempc0),len(trim(tempc0)))
                bface(i)%c1=Hex2Dec(trim(tempc1),len(trim(tempc1)))
            end do
        end if
            
        if(currentString(1:6)=="(13(5")then
            !temp=currentString(10:11)
            faces5=Hex2Dec(currentString(10:11),2)
            do i=faces4+1,faces5
                !read(gridId,*,iostat=error) bface(i)%nNodes,bface(i)%nodelist(1),bface(i)%nodelist(2),bface(i)%c0,bface(i)%c1
                read(gridId,*,iostat=error) bface(i)%nNodes,temp,temp1,tempc0,tempc1
                allocate(bface(i)%nodelist(bface(i)%nNodes))
                bface(i)%nodelist(1)=Hex2Dec(trim(temp),len(trim(temp)))
                bface(i)%nodelist(2)=Hex2Dec(trim(temp1),len(trim(temp1)))
                bface(i)%c0=Hex2Dec(trim(tempc0),len(trim(tempc0)))
                bface(i)%c1=Hex2Dec(trim(tempc1),len(trim(tempc1)))
            end do
        end if
            
        if(currentString(1:6)=="(13(6")then
            !temp=currentString(10:11)
            faces6=Hex2Dec(currentString(10:11),2)
            do i=faces5+1,faces6
                !read(gridId,*,iostat=error) bface(i)%nNodes,bface(i)%nodelist(1),bface(i)%nodelist(2),bface(i)%c0,bface(i)%c1
                read(gridId,*,iostat=error) bface(i)%nNodes,temp,temp1,tempc0,tempc1
                allocate(bface(i)%nodelist(bface(i)%nNodes))
                bface(i)%nodelist(1)=Hex2Dec(trim(temp),len(trim(temp)))
                bface(i)%nodelist(2)=Hex2Dec(trim(temp1),len(trim(temp1)))
                bface(i)%c0=Hex2Dec(trim(tempc0),len(trim(tempc0)))
                bface(i)%c1=Hex2Dec(trim(tempc1),len(trim(tempc1)))
            end do
        end if
            
        if(currentString(1:6)=="(13(8")then
            !temp=currentString(10:12)
            faces8=Hex2Dec(currentString(10:12),3)
            allocate(face(1316))
            do i=faces6+1-80,faces8-80
                !read(gridId,*,iostat=error) face(i)%nNodes,face(i)%nodelist(1),face(i)%nodelist(2),face(i)%c0,face(i)%c1
                read(gridId,*,iostat=error) face(i)%nNodes,temp,temp1,tempc0,tempc1
                allocate(face(i)%nodelist(face(i)%nNodes))
                face(i)%nodelist(1)=Hex2Dec(trim(temp),len(trim(temp)))
                face(i)%nodelist(2)=Hex2Dec(trim(temp1),len(trim(temp1)))
                face(i)%c0=Hex2Dec(trim(tempc0),len(trim(tempc0)))
                face(i)%c1=Hex2Dec(trim(tempc1),len(trim(tempc1)))
            end do
        end if
            
        if(currentString(1:6)=="(12 (0")then
            !temp=currentString(10:12)
            ncells=Hex2Dec(currentString(10:12),3)
            allocate(cell(ncells))
        end if
        
        if(currentString(1:6)=="(12 (2")  exit
            
    end do
    
   call gridReconstruction
    
end subroutine input_msh