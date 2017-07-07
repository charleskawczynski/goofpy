       module ARRAYS_mod
       use module_needed_for_array_mod
       use array_mod
       implicit none

       private
       public :: init,delete,display,print,export,import

       interface init;   module interface init_arrays;   end interface
       interface delete; module interface delete_arrays; end interface
       interface display;module interface display_arrays;end interface
       interface print;  module interface print_arrays;  end interface
       interface export; module interface export_arrays; end interface
       interface import; module interface import_arrays; end interface

       type ARRAYS
         private
         integer :: n = 0
         type(array),dimension(:),allocatable :: a
       end type

       contains

       subroutine init_ARRAYS(this,that)
         implicit none
         type(arrays),intent(inout) :: this
         type(arrays),intent(in) :: that
         integer :: i_iter
         call delete(this)
         this%n = that%n
         if (allocated(that%a)) then
           allocate(this%a(size(that%a)))
           do i_iter=1,size(this%a)
             call init(this%a(i_iter),that%a(i_iter))
           enddo
         endif
       end subroutine

       subroutine init_many_ARRAYS(this,that)
         implicit none
         type(arrays),dimension(:),intent(inout) :: this
         type(arrays),dimension(:),intent(in) :: that
         integer :: i_iter
         if (allocated(that)) then
           allocate(this(size(that)))
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_ARRAYS(this)
         implicit none
         type(arrays),intent(inout) :: this
         integer :: i_iter
         this%n = 0
         if (allocated(this%a)) then
           do i_iter=1,size(this%a)
             call delete(this%a(i))
           enddo
           deallocate(this%a)
         endif
       end subroutine

       subroutine delete_many_ARRAYS(this)
         implicit none
         type(arrays),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
           deallocate(this)
         endif
       end subroutine

       subroutine display_ARRAYS(this,un)
         implicit none
         type(arrays),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n = ',this%n
         call display(this%a,un)
       end subroutine

       subroutine display_many_ARRAYS(this,un)
         implicit none
         type(arrays),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_ARRAYS(this)
         implicit none
         type(arrays),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_ARRAYS(this)
         implicit none
         type(arrays),dimension(:),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_ARRAYS(this,un)
         implicit none
         type(arrays),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%n
         call export(this%a,un)
       end subroutine

       subroutine import_ARRAYS(this,un)
         implicit none
         type(arrays),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%n
         call import(this%a,un)
       end subroutine

       end module