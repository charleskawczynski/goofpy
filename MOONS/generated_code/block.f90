       module BLOCK_mod
       use IO_tools_mod
       use grid_mod
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
       public :: BLOCK
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_block;          end interface
       interface init;   module procedure init_many_block;     end interface
       interface delete; module procedure delete_block;        end interface
       interface delete; module procedure delete_many_block;   end interface
       interface display;module procedure display_block;       end interface
       interface display;module procedure display_many_block;  end interface
       interface print;  module procedure print_block;         end interface
       interface print;  module procedure print_many_block;    end interface
       interface export; module procedure export_block;        end interface
       interface export; module procedure export_many_block;   end interface
       interface import; module procedure import_block;        end interface
       interface import; module procedure import_many_block;   end interface
       interface export; module procedure export_wrapper_block;end interface
       interface import; module procedure import_wrapper_block;end interface

       type BLOCK
         type(grid),dimension(3) :: g
       end type

       contains

       subroutine init_BLOCK(this,that)
         implicit none
         type(block),intent(inout) :: this
         type(block),intent(in) :: that
         call delete(this)
         call init(this%g,that%g)
       end subroutine

       subroutine init_many_BLOCK(this,that)
         implicit none
         type(block),dimension(:),intent(inout) :: this
         type(block),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_BLOCK(this)
         implicit none
         type(block),intent(inout) :: this
         call delete(this%g)
       end subroutine

       subroutine delete_many_BLOCK(this)
         implicit none
         type(block),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_BLOCK(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         call display(this%g,un)
       end subroutine

       subroutine display_many_BLOCK(this,un)
         implicit none
         type(block),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_BLOCK(this)
         implicit none
         type(block),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_BLOCK(this)
         implicit none
         type(block),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_BLOCK(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         call export(this%g,un)
       end subroutine

       subroutine export_many_BLOCK(this,un)
         implicit none
         type(block),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_BLOCK(this,un)
         implicit none
         type(block),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%g,un)
       end subroutine

       subroutine import_many_BLOCK(this,un)
         implicit none
         type(block),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_BLOCK(this,dir,name)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_BLOCK(this,dir,name)
         implicit none
         type(block),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module