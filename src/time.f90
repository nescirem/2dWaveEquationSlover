program TimeNow
   character*8 :: now
   integer     :: hh,mm,ss

   call time (now)     ! 读系统时间
   write(*,*) now      ! 写出时间字符串

   ! 用字符变量now作为内部文件，从中将时、分、秒作为整数读出
   read(now,"(I2,1x,I2,1x,I2)") hh,mm,ss  ! 字符转换为
   write(*,*) hh,mm,ss ! 写出时、分、秒的整数值

   stop
end program TimeNow


   write(now,"(2I1,1H:,I2,1H:,2I1)") 0,1,35,0,5