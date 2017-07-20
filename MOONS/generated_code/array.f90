       module ARRAY_mod
       use IO_tools_mod
       implicit none

#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif
       private
       public :: ARRAY
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_array;          end interface
       interface init;   module procedure init_many_array;     end interface
       interface delete; module procedure delete_array;        end interface
       interface delete; module procedure delete_many_array;   end interface
       interface display;module procedure display_array;       end interface
       interface display;module procedure display_many_array;  end interface
       interface print;  module procedure print_array;         end interface
       interface print;  module procedure print_many_array;    end interface
       interface export; module procedure export_array;        end interface
       interface export; module procedure export_many_array;   end interface
       interface import; module procedure import_array;        end interface
       interface import; module procedure import_many_array;   end interface
       interface export; module procedure export_wrapper_array;end interface
       interface import; module procedure import_wrapper_array;end interface

       type ARRAY
         real(cp),dimension(:),allocatable :: f
         integer :: n = 0
       end type

       contains

       subroutine init_ARRAY(this,that)
         implicit none
         type(array),intent(inout) :: this
         type(array),intent(in) :: that
         call delete(this)
         if (allocated(that%f)) then
           allocate(this%f(size(that%f)))
           this%f = that%f
         endif
         this%n = that%n
       end subroutine

       subroutine init_many_ARRAY(this,that)
         implicit none
         type(array),dimension(:),intent(inout) :: this
         type(array),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_ARRAY(this)
         implicit none
         type(array),intent(inout) :: this
         if (allocated(this%f)) then
           this%f = 0.0_cp
           deallocate(this%f)
         endif
         this%n = 0
       end subroutine

       subroutine delete_many_ARRAY(this)
         implicit none
         type(array),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_ARRAY(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'f = ',this%f
         write(un,*) 'n = ',this%n
       end subroutine

       subroutine display_many_ARRAY(this,un)
         implicit none
         type(array),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
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
         type(array),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_ARRAY(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%f
         write(un,*) this%n
       end subroutine

       subroutine export_many_ARRAY(this,un)
         implicit none
         type(array),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_ARRAY(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%f
         read(un,*) this%n
       end subroutine

       subroutine import_many_ARRAY(this,un)
         implicit none
         type(array),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_ARRAY(this,dir,name)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_ARRAY(this,dir,name)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module