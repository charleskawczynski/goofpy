       module MESH_BLOCK_mod
       use IO_tools_mod
       use block_mod
       use mesh_mod
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
       public :: MESH_BLOCK
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_block;          end interface
       interface init;   module procedure init_many_mesh_block;     end interface
       interface delete; module procedure delete_mesh_block;        end interface
       interface delete; module procedure delete_many_mesh_block;   end interface
       interface display;module procedure display_mesh_block;       end interface
       interface display;module procedure display_many_mesh_block;  end interface
       interface print;  module procedure print_mesh_block;         end interface
       interface print;  module procedure print_many_mesh_block;    end interface
       interface export; module procedure export_mesh_block;        end interface
       interface export; module procedure export_many_mesh_block;   end interface
       interface import; module procedure import_mesh_block;        end interface
       interface import; module procedure import_many_mesh_block;   end interface
       interface export; module procedure export_wrapper_mesh_block;end interface
       interface import; module procedure import_wrapper_mesh_block;end interface

       type MESH_BLOCK
         private
         type(mesh) :: m
         type(block) :: b
       end type

       contains

       subroutine init_MESH_BLOCK(this,that)
         implicit none
         type(mesh_block),intent(inout) :: this
         type(mesh_block),intent(in) :: that
         call delete(this)
         call init(this%m,that%m)
         call init(this%b,that%b)
       end subroutine

       subroutine init_many_MESH_BLOCK(this,that)
         implicit none
         type(mesh_block),dimension(:),intent(inout) :: this
         type(mesh_block),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MESH_BLOCK(this)
         implicit none
         type(mesh_block),intent(inout) :: this
         call delete(this%m)
         call delete(this%b)
       end subroutine

       subroutine delete_many_MESH_BLOCK(this)
         implicit none
         type(mesh_block),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),intent(in) :: this
         integer,intent(in) :: un
         call display(this%m,un)
         call display(this%b,un)
       end subroutine

       subroutine display_many_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MESH_BLOCK(this)
         implicit none
         type(mesh_block),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MESH_BLOCK(this)
         implicit none
         type(mesh_block),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),intent(in) :: this
         integer,intent(in) :: un
         call export(this%m,un)
         call export(this%b,un)
       end subroutine

       subroutine export_many_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%m,un)
         call import(this%b,un)
       end subroutine

       subroutine import_many_MESH_BLOCK(this,un)
         implicit none
         type(mesh_block),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MESH_BLOCK(this,dir,name)
         implicit none
         type(mesh_block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MESH_BLOCK(this,dir,name)
         implicit none
         type(mesh_block),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module