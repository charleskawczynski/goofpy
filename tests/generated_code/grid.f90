       module GRID_mod
       use coordinates_mod
       implicit none

       private
       public :: init,delete,display,print,export,import

       interface init;   module interface init_grid;          end interface
       interface delete; module interface delete_grid;        end interface
       interface display;module interface display_grid;       end interface
       interface print;  module interface print_grid;         end interface
       interface export; module interface export_grid;        end interface
       interface import; module interface import_grid;        end interface
       interface export; module interface export_wrapper_grid;end interface
       interface import; module interface import_wrapper_grid;end interface

       type GRID
         private
         type(coordinates),dimension(3) :: c
       end type

       contains

       subroutine init_GRID(this,that)
         implicit none
         type(grid),intent(inout) :: this
         type(grid),intent(in) :: that
         call delete(this)
         call init(this%c,that%c)
       end subroutine

       subroutine init_many_GRID(this,that)
         implicit none
         type(grid),dimension(:),intent(inout) :: this
         type(grid),dimension(:),intent(in) :: that
         integer :: i_iter
         if (allocated(that)) then
           allocate(this(size(that)))
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_GRID(this)
         implicit none
         type(grid),intent(inout) :: this
         call delete(this%c)
       end subroutine

       subroutine delete_many_GRID(this)
         implicit none
         type(grid),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
           deallocate(this)
         endif
       end subroutine

       subroutine display_GRID(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         call display(this%c,un)
       end subroutine

       subroutine display_many_GRID(this,un)
         implicit none
         type(grid),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (allocated(this)) then
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
         type(grid),dimension(:),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_GRID(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         call export(this%c,un)
       end subroutine

       subroutine import_GRID(this,un)
         implicit none
         type(grid),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%c,un)
       end subroutine

       end module