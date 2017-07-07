       module COORDINATES_mod
       use array_mod
       implicit none

       private
       public :: init,delete,display,print,export,import

       interface init;   module interface init_coordinates;          end interface
       interface delete; module interface delete_coordinates;        end interface
       interface display;module interface display_coordinates;       end interface
       interface print;  module interface print_coordinates;         end interface
       interface export; module interface export_coordinates;        end interface
       interface import; module interface import_coordinates;        end interface
       interface export; module interface export_wrapper_coordinates;end interface
       interface import; module interface import_wrapper_coordinates;end interface

       type COORDINATES
         private
         type(array) :: hn
         type(array) :: hc
       end type

       contains

       subroutine init_COORDINATES(this,that)
         implicit none
         type(coordinates),intent(inout) :: this
         type(coordinates),intent(in) :: that
         call delete(this)
         call init(this%hn,that%hn)
         call init(this%hc,that%hc)
       end subroutine

       subroutine init_many_COORDINATES(this,that)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         type(coordinates),dimension(:),intent(in) :: that
         integer :: i_iter
         if (allocated(that)) then
           allocate(this(size(that)))
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
       end subroutine

       subroutine delete_many_COORDINATES(this)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
           deallocate(this)
         endif
       end subroutine

       subroutine display_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         call display(this%hn,un)
         call display(this%hc,un)
       end subroutine

       subroutine display_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (allocated(this)) then
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
         type(coordinates),dimension(:),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         call export(this%hn,un)
         call export(this%hc,un)
       end subroutine

       subroutine import_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%hn,un)
         call import(this%hc,un)
       end subroutine

       end module