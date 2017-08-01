       module BOUNDARY_mod
       use IO_tools_mod
       use BC_logicals_mod
       use string_mod
       use single_boundary_mod
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
       public :: BOUNDARY
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_boundary;          end interface
       interface init;   module procedure init_many_boundary;     end interface
       interface delete; module procedure delete_boundary;        end interface
       interface delete; module procedure delete_many_boundary;   end interface
       interface display;module procedure display_boundary;       end interface
       interface display;module procedure display_many_boundary;  end interface
       interface print;  module procedure print_boundary;         end interface
       interface print;  module procedure print_many_boundary;    end interface
       interface export; module procedure export_boundary;        end interface
       interface export; module procedure export_many_boundary;   end interface
       interface import; module procedure import_boundary;        end interface
       interface import; module procedure import_many_boundary;   end interface
       interface export; module procedure export_wrapper_boundary;end interface
       interface import; module procedure import_wrapper_boundary;end interface

       type BOUNDARY
         private
         integer :: n = 0
         type(single_boundary),dimension(:),allocatable :: sb
         type(string) :: name
         type(bc_logicals) :: bcl
       end type

       contains

       subroutine init_BOUNDARY(this,that)
         implicit none
         type(boundary),intent(inout) :: this
         type(boundary),intent(in) :: that
         integer :: i_iter
         call delete(this)
         this%n = that%n
         if (allocated(that%sb)) then
           allocate(this%sb(size(that%sb)))
           do i_iter=1,size(this%sb)
             call init(this%sb(i_iter),that%sb(i_iter))
           enddo
         endif
         call init(this%name,that%name)
         call init(this%bcl,that%bcl)
       end subroutine

       subroutine init_many_BOUNDARY(this,that)
         implicit none
         type(boundary),dimension(:),intent(inout) :: this
         type(boundary),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_BOUNDARY(this)
         implicit none
         type(boundary),intent(inout) :: this
         integer :: i_iter
         this%n = 0
         if (allocated(this%sb)) then
           do i_iter=1,size(this%sb)
             call delete(this%sb(i_iter))
           enddo
           deallocate(this%sb)
         endif
         call delete(this%name)
         call delete(this%bcl)
       end subroutine

       subroutine delete_many_BOUNDARY(this)
         implicit none
         type(boundary),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_BOUNDARY(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n    = ',this%n
         call display(this%sb,un)
         call display(this%name,un)
         call display(this%bcl,un)
       end subroutine

       subroutine display_many_BOUNDARY(this,un)
         implicit none
         type(boundary),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_BOUNDARY(this)
         implicit none
         type(boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_BOUNDARY(this)
         implicit none
         type(boundary),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_BOUNDARY(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%n
         call export(this%sb,un)
         call export(this%name,un)
         call export(this%bcl,un)
       end subroutine

       subroutine export_many_BOUNDARY(this,un)
         implicit none
         type(boundary),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_BOUNDARY(this,un)
         implicit none
         type(boundary),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%n
         call import(this%sb,un)
         call import(this%name,un)
         call import(this%bcl,un)
       end subroutine

       subroutine import_many_BOUNDARY(this,un)
         implicit none
         type(boundary),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_BOUNDARY(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_BOUNDARY(this,dir,name)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module