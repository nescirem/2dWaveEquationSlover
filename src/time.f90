program TimeNow
   character*8 :: now
   integer     :: hh,mm,ss

   call time (now)     ! ��ϵͳʱ��
   write(*,*) now      ! д��ʱ���ַ���

   ! ���ַ�����now��Ϊ�ڲ��ļ������н�ʱ���֡�����Ϊ��������
   read(now,"(I2,1x,I2,1x,I2)") hh,mm,ss  ! �ַ�ת��Ϊ
   write(*,*) hh,mm,ss ! д��ʱ���֡��������ֵ

   stop
end program TimeNow


   write(now,"(2I1,1H:,I2,1H:,2I1)") 0,1,35,0,5