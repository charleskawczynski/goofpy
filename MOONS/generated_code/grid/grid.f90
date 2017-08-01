       module GRID_mod
       use IO_tools_mod
       use coordinates_mod
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
       public :: GRID
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_grid;          end interface
       interface init;   module procedure init_many_grid;     end interface
       interface delete; module procedure delete_grid;        end interface
       interface delete; module procedure delete_many_grid;   end interface
       interface display;module procedure display_grid;       end interface
       interface display;module procedure display_many_grid;  end interface
       interface print;  module procedure print_grid;         end interface
       interface print;  module procedure print_many_grid;    end interface
       interface export; module procedure export_grid;        end interface
       interface export; module procedure export_many_grid;   end interface
       interface import; module procedure import_grid;        end interface
       interface import; module procedure import_many_grid;   end interface
       interface export; module procedure export_wrapper_grid;end interface
       interface import; module procedure import_wrapper_grid;end interface

       type GRID
         private
         type(coordinates),dimension(3) :: c
         real(cp) :: volume = 0.0_cp
         logical :: defined = .false.
       end type

       contains

       subroutine init_GRID(this,that)
         implicit none
         type(grid),intent(inout) :: this
         type(grid),intent(in) :: that
         call delete(this)
         call init(this%c,that%c)
         this%volume = that%volume
         this%defined = that%defined
       end subroutine

       subroutine init_many_GRID(this,that)
         implicit none
         type(grid),dimension(:),intent(inout) :: this
         type(grid),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_GRID(this)
         implicit none
         type(grid),intent(inout) :: this
         call delete(this%c)
         this%volume = 0.0_cp
         this%defined = .false.
       end subroutine

       subroutine delete_many_GRID(this)
         implicit none
         type(grid),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_GRID(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         call display(this%c,un)
         write(un,*) 'volume  = ',this%volume
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_many_GRID(this,un)
         implicit none
         type(grid),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_GRID(this)
         implicit none
         type(grid),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_GRID(this)
         implicit none
         type(grid),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_GRID(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         call export(this%c,un)
         write(un,*) this%volume
         write(un,*) this%defined
       end subroutine

       subroutine export_many_GRID(this,un)
         implicit none
         type(grid),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_GRID(this,un)
         implicit none
         type(grid),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%c,un)
         read(un,*) this%volume
         read(un,*) this%defined
       end subroutine

       subroutine import_many_GRID(this,un)
         implicit none
         type(grid),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_GRID(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_GRID(this,dir,name)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module