       module GRID_FIELD_mod
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
       public :: GRID_FIELD
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_grid_field;          end interface
       interface init;   module procedure init_many_grid_field;     end interface
       interface delete; module procedure delete_grid_field;        end interface
       interface delete; module procedure delete_many_grid_field;   end interface
       interface display;module procedure display_grid_field;       end interface
       interface display;module procedure display_many_grid_field;  end interface
       interface print;  module procedure print_grid_field;         end interface
       interface print;  module procedure print_many_grid_field;    end interface
       interface export; module procedure export_grid_field;        end interface
       interface export; module procedure export_many_grid_field;   end interface
       interface import; module procedure import_grid_field;        end interface
       interface import; module procedure import_many_grid_field;   end interface
       interface export; module procedure export_wrapper_grid_field;end interface
       interface import; module procedure import_wrapper_grid_field;end interface

       type GRID_FIELD
         real(cp),dimension(:,:,:),allocatable :: f
         integer,dimension(3) :: s = 0
         integer :: s_1d = 0
       end type

       contains

       subroutine init_GRID_FIELD(this,that)
         implicit none
         type(grid_field),intent(inout) :: this
         type(grid_field),intent(in) :: that
         call delete(this)
         if (allocated(that%f)) then
           allocate(this%f(size(that%f)))
           this%f = that%f
         endif
         this%s = that%s
         this%s_1d = that%s_1d
       end subroutine

       subroutine init_many_GRID_FIELD(this,that)
         implicit none
         type(grid_field),dimension(:),intent(inout) :: this
         type(grid_field),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_GRID_FIELD(this)
         implicit none
         type(grid_field),intent(inout) :: this
         if (allocated(this%f)) then
           this%f = 0
           deallocate(this%f)
         endif
         this%s = 0
         this%s_1d = 0
       end subroutine

       subroutine delete_many_GRID_FIELD(this)
         implicit none
         type(grid_field),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_GRID_FIELD(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'f = ',this%f
         write(un,*) 's = ',this%s
         write(un,*) 's_1d = ',this%s_1d
       end subroutine

       subroutine display_many_GRID_FIELD(this,un)
         implicit none
         type(grid_field),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_GRID_FIELD(this)
         implicit none
         type(grid_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_GRID_FIELD(this)
         implicit none
         type(grid_field),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_GRID_FIELD(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%f
         write(un,*) this%s
         write(un,*) this%s_1d
       end subroutine

       subroutine export_many_GRID_FIELD(this,un)
         implicit none
         type(grid_field),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_GRID_FIELD(this,un)
         implicit none
         type(grid_field),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%f
         read(un,*) this%s
         read(un,*) this%s_1d
       end subroutine

       subroutine import_many_GRID_FIELD(this,un)
         implicit none
         type(grid_field),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_GRID_FIELD(this,dir,name)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_GRID_FIELD(this,dir,name)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module