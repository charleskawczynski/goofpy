       module COORDINATES_mod
       use IO_tools_mod
       use array_mod
       use sparse_mod
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
       public :: COORDINATES
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_coordinates;          end interface
       interface init;   module procedure init_many_coordinates;     end interface
       interface delete; module procedure delete_coordinates;        end interface
       interface delete; module procedure delete_many_coordinates;   end interface
       interface display;module procedure display_coordinates;       end interface
       interface display;module procedure display_many_coordinates;  end interface
       interface print;  module procedure print_coordinates;         end interface
       interface print;  module procedure print_many_coordinates;    end interface
       interface export; module procedure export_coordinates;        end interface
       interface export; module procedure export_many_coordinates;   end interface
       interface import; module procedure import_coordinates;        end interface
       interface import; module procedure import_many_coordinates;   end interface
       interface export; module procedure export_wrapper_coordinates;end interface
       interface import; module procedure import_wrapper_coordinates;end interface

       type COORDINATES
         type(array) :: hn
         type(array) :: hc
         type(sparse),dimension(2) :: col
         type(sparse),dimension(2) :: stag
       end type

       contains

       subroutine init_COORDINATES(this,that)
         implicit none
         type(coordinates),intent(inout) :: this
         type(coordinates),intent(in) :: that
         call delete(this)
         call init(this%hn,that%hn)
         call init(this%hc,that%hc)
         call init(this%col,that%col)
         call init(this%stag,that%stag)
       end subroutine

       subroutine init_many_COORDINATES(this,that)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         type(coordinates),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_COORDINATES(this)
         implicit none
         type(coordinates),intent(inout) :: this
         call delete(this%hn)
         call delete(this%hc)
         call delete(this%col)
         call delete(this%stag)
       end subroutine

       subroutine delete_many_COORDINATES(this)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%col,un)
         call display(this%stag,un)
       end subroutine

       subroutine display_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_COORDINATES(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_COORDINATES(this)
         implicit none
         type(coordinates),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         call export(this%hn,un)
         call export(this%hc,un)
         call export(this%col,un)
         call export(this%stag,un)
       end subroutine

       subroutine export_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%hn,un)
         call import(this%hc,un)
         call import(this%col,un)
         call import(this%stag,un)
       end subroutine

       subroutine import_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_COORDINATES(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_COORDINATES(this,dir,name)
         implicit none
         type(coordinates),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module