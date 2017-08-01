       module PROCEDURE_ARRAY_mod
       use IO_tools_mod
       use single_procedure_mod
       implicit none

       integer,parameter :: li = selected_int_kind(16)
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
       public :: PROCEDURE_ARRAY
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_procedure_array;          end interface
       interface init;   module procedure init_many_procedure_array;     end interface
       interface delete; module procedure delete_procedure_array;        end interface
       interface delete; module procedure delete_many_procedure_array;   end interface
       interface display;module procedure display_procedure_array;       end interface
       interface display;module procedure display_many_procedure_array;  end interface
       interface print;  module procedure print_procedure_array;         end interface
       interface print;  module procedure print_many_procedure_array;    end interface
       interface export; module procedure export_procedure_array;        end interface
       interface export; module procedure export_many_procedure_array;   end interface
       interface import; module procedure import_procedure_array;        end interface
       interface import; module procedure import_many_procedure_array;   end interface
       interface export; module procedure export_wrapper_procedure_array;end interface
       interface import; module procedure import_wrapper_procedure_array;end interface

       type PROCEDURE_ARRAY
         private
         integer :: n = 0
         type(single_procedure),dimension(:),allocatable :: sp
         logical :: defined = .false.
       end type

       contains

       subroutine init_PROCEDURE_ARRAY(this,that)
         implicit none
         type(procedure_array),intent(inout) :: this
         type(procedure_array),intent(in) :: that
         integer :: i_iter
         call delete(this)
         this%n = that%n
         if (allocated(that%sp)) then
           allocate(this%sp(size(that%sp)))
           do i_iter=1,size(this%sp)
             call init(this%sp(i_iter),that%sp(i_iter))
           enddo
         endif
         this%defined = that%defined
       end subroutine

       subroutine init_many_PROCEDURE_ARRAY(this,that)
         implicit none
         type(procedure_array),dimension(:),intent(inout) :: this
         type(procedure_array),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_PROCEDURE_ARRAY(this)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer :: i_iter
         this%n = 0
         if (allocated(this%sp)) then
           do i_iter=1,size(this%sp)
             call delete(this%sp(i_iter))
           enddo
           deallocate(this%sp)
         endif
         this%defined = .false.
       end subroutine

       subroutine delete_many_PROCEDURE_ARRAY(this)
         implicit none
         type(procedure_array),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n       = ',this%n
         call display(this%sp,un)
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_many_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_PROCEDURE_ARRAY(this)
         implicit none
         type(procedure_array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_PROCEDURE_ARRAY(this)
         implicit none
         type(procedure_array),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%n
         call export(this%sp,un)
         write(un,*) this%defined
       end subroutine

       subroutine export_many_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%n
         call import(this%sp,un)
         read(un,*) this%defined
       end subroutine

       subroutine import_many_PROCEDURE_ARRAY(this,un)
         implicit none
         type(procedure_array),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_PROCEDURE_ARRAY(this,dir,name)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_PROCEDURE_ARRAY(this,dir,name)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module