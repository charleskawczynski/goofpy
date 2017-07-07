       module ARRAY_mod
       use IO_tools_mod
       implicit none

       private
       public :: init,delete,display,print,export,import

       interface init;   module interface init_array;          end interface
       interface delete; module interface delete_array;        end interface
       interface display;module interface display_array;       end interface
       interface print;  module interface print_array;         end interface
       interface export; module interface export_array;        end interface
       interface import; module interface import_array;        end interface
       interface export; module interface export_wrapper_array;end interface
       interface import; module interface import_wrapper_array;end interface

       type ARRAY
         private
         real(cp),dimension(:),allocatable :: a
         integer :: n = 0
       end type

       contains

       subroutine init_ARRAY(this,that)
         implicit none
         type(array),intent(inout) :: this
         type(array),intent(in) :: that
         call delete(this)
         if (allocated(that%a)) then
           allocate(this%a(size(that%a)))
           this%a = that%a
         endif
         this%n = that%n
       end subroutine

       subroutine init_many_ARRAY(this,that)
         implicit none
         type(array),dimension(:),intent(inout) :: this
         type(array),dimension(:),intent(in) :: that
         integer :: i_iter
         if (allocated(that)) then
           allocate(this(size(that)))
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_ARRAY(this)
         implicit none
         type(array),intent(inout) :: this
         if (allocated(this%a)) then
           this%a = 0.0_cp
           deallocate(this%a)
         endif
         this%n = 0
       end subroutine

       subroutine delete_many_ARRAY(this)
         implicit none
         type(array),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
           deallocate(this)
         endif
       end subroutine

       subroutine display_ARRAY(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'a = ',this%a
         write(un,*) 'n = ',this%n
       end subroutine

       subroutine display_many_ARRAY(this,un)
         implicit none
         type(array),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_ARRAY(this)
         implicit none
         type(array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_ARRAY(this)
         implicit none
         type(array),dimension(:),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_ARRAY(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%a
         write(un,*) this%n
       end subroutine

       subroutine import_ARRAY(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%a
         read(un,*) this%n
       end subroutine

       end module