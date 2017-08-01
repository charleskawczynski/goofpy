       module SINGLE_BOUNDARY_mod
       use IO_tools_mod
       use bctype_mod
       use grid_field_mod
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
       public :: SINGLE_BOUNDARY
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_single_boundary;          end interface
       interface init;   module procedure init_many_single_boundary;     end interface
       interface delete; module procedure delete_single_boundary;        end interface
       interface delete; module procedure delete_many_single_boundary;   end interface
       interface display;module procedure display_single_boundary;       end interface
       interface display;module procedure display_many_single_boundary;  end interface
       interface print;  module procedure print_single_boundary;         end interface
       interface print;  module procedure print_many_single_boundary;    end interface
       interface export; module procedure export_single_boundary;        end interface
       interface export; module procedure export_many_single_boundary;   end interface
       interface import; module procedure import_single_boundary;        end interface
       interface import; module procedure import_many_single_boundary;   end interface
       interface export; module procedure export_wrapper_single_boundary;end interface
       interface import; module procedure import_wrapper_single_boundary;end interface

       type SINGLE_BOUNDARY
         private
         type(bctype) :: bct
         type(grid_field) :: b
         type(grid_field) :: b_modified
         type(grid_field) :: b_total
       end type

       contains

       subroutine init_SINGLE_BOUNDARY(this,that)
         implicit none
         type(single_boundary),intent(inout) :: this
         type(single_boundary),intent(in) :: that
         call delete(this)
         call init(this%bct,that%bct)
         call init(this%b,that%b)
         call init(this%b_modified,that%b_modified)
         call init(this%b_total,that%b_total)
       end subroutine

       subroutine init_many_SINGLE_BOUNDARY(this,that)
         implicit none
         type(single_boundary),dimension(:),intent(inout) :: this
         type(single_boundary),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SINGLE_BOUNDARY(this)
         implicit none
         type(single_boundary),intent(inout) :: this
         call delete(this%bct)
         call delete(this%b)
         call delete(this%b_modified)
         call delete(this%b_total)
       end subroutine

       subroutine delete_many_SINGLE_BOUNDARY(this)
         implicit none
         type(single_boundary),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call display(this%bct,un)
         call display(this%b,un)
         call display(this%b_modified,un)
         call display(this%b_total,un)
       end subroutine

       subroutine display_many_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SINGLE_BOUNDARY(this)
         implicit none
         type(single_boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SINGLE_BOUNDARY(this)
         implicit none
         type(single_boundary),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),intent(in) :: this
         integer,intent(in) :: un
         call export(this%bct,un)
         call export(this%b,un)
         call export(this%b_modified,un)
         call export(this%b_total,un)
       end subroutine

       subroutine export_many_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%bct,un)
         call import(this%b,un)
         call import(this%b_modified,un)
         call import(this%b_total,un)
       end subroutine

       subroutine import_many_SINGLE_BOUNDARY(this,un)
         implicit none
         type(single_boundary),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SINGLE_BOUNDARY(this,dir,name)
         implicit none
         type(single_boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SINGLE_BOUNDARY(this,dir,name)
         implicit none
         type(single_boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module