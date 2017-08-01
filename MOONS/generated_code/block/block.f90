       module BLOCK_mod
       use IO_tools_mod
       use grid_mod
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
         private
         type(grid),dimension(3) :: g
         type(grid),dimension(:),allocatable :: f
         type(grid),dimension(:),allocatable :: e
         type(grid),dimension(:),allocatable :: c
         type(grid),dimension(:),allocatable :: fb
         type(grid),dimension(:),allocatable :: eb
         type(grid),dimension(:),allocatable :: cb
         type(grid_field),dimension(:),allocatable :: vol
         integer,dimension(6) :: apply_bc_order = 0
       end type

       contains

       subroutine init_BLOCK(this,that)
         implicit none
         type(block),intent(inout) :: this
         type(block),intent(in) :: that
         integer :: i_iter
         call delete(this)
         call init(this%g,that%g)
         if (allocated(that%f)) then
           allocate(this%f(size(that%f)))
           do i_iter=1,size(this%f)
             call init(this%f(i_iter),that%f(i_iter))
           enddo
         endif
         if (allocated(that%e)) then
           allocate(this%e(size(that%e)))
           do i_iter=1,size(this%e)
             call init(this%e(i_iter),that%e(i_iter))
           enddo
         endif
         if (allocated(that%c)) then
           allocate(this%c(size(that%c)))
           do i_iter=1,size(this%c)
             call init(this%c(i_iter),that%c(i_iter))
           enddo
         endif
         if (allocated(that%fb)) then
           allocate(this%fb(size(that%fb)))
           do i_iter=1,size(this%fb)
             call init(this%fb(i_iter),that%fb(i_iter))
           enddo
         endif
         if (allocated(that%eb)) then
           allocate(this%eb(size(that%eb)))
           do i_iter=1,size(this%eb)
             call init(this%eb(i_iter),that%eb(i_iter))
           enddo
         endif
         if (allocated(that%cb)) then
           allocate(this%cb(size(that%cb)))
           do i_iter=1,size(this%cb)
             call init(this%cb(i_iter),that%cb(i_iter))
           enddo
         endif
         if (allocated(that%vol)) then
           allocate(this%vol(size(that%vol)))
           do i_iter=1,size(this%vol)
             call init(this%vol(i_iter),that%vol(i_iter))
           enddo
         endif
         this%apply_bc_order = that%apply_bc_order
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
         integer :: i_iter
         call delete(this%g)
         if (allocated(this%f)) then
           do i_iter=1,size(this%f)
             call delete(this%f(i_iter))
           enddo
           deallocate(this%f)
         endif
         if (allocated(this%e)) then
           do i_iter=1,size(this%e)
             call delete(this%e(i_iter))
           enddo
           deallocate(this%e)
         endif
         if (allocated(this%c)) then
           do i_iter=1,size(this%c)
             call delete(this%c(i_iter))
           enddo
           deallocate(this%c)
         endif
         if (allocated(this%fb)) then
           do i_iter=1,size(this%fb)
             call delete(this%fb(i_iter))
           enddo
           deallocate(this%fb)
         endif
         if (allocated(this%eb)) then
           do i_iter=1,size(this%eb)
             call delete(this%eb(i_iter))
           enddo
           deallocate(this%eb)
         endif
         if (allocated(this%cb)) then
           do i_iter=1,size(this%cb)
             call delete(this%cb(i_iter))
           enddo
           deallocate(this%cb)
         endif
         if (allocated(this%vol)) then
           do i_iter=1,size(this%vol)
             call delete(this%vol(i_iter))
           enddo
           deallocate(this%vol)
         endif
         this%apply_bc_order = 0
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
         call display(this%f,un)
         call display(this%e,un)
         call display(this%c,un)
         call display(this%fb,un)
         call display(this%eb,un)
         call display(this%cb,un)
         call display(this%vol,un)
         write(un,*) 'apply_bc_order = ',this%apply_bc_order
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
         call export(this%f,un)
         call export(this%e,un)
         call export(this%c,un)
         call export(this%fb,un)
         call export(this%eb,un)
         call export(this%cb,un)
         call export(this%vol,un)
         write(un,*) this%apply_bc_order
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
         call import(this%f,un)
         call import(this%e,un)
         call import(this%c,un)
         call import(this%fb,un)
         call import(this%eb,un)
         call import(this%cb,un)
         call import(this%vol,un)
         read(un,*) this%apply_bc_order
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