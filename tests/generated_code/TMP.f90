       module TMP_mod
       use used_modB1_mod
       use used_modB2_mod
       use grid_mod
       implicit none

       private
       public :: init,delete,display,print,export,import

       interface init;   module interface init_tmp;          end interface
       interface delete; module interface delete_tmp;        end interface
       interface display;module interface display_tmp;       end interface
       interface print;  module interface print_tmp;         end interface
       interface export; module interface export_tmp;        end interface
       interface import; module interface import_tmp;        end interface
       interface export; module interface export_wrapper_tmp;end interface
       interface import; module interface import_wrapper_tmp;end interface

       type TMP
         private
         type(grid) :: g
         type(grid),dimension(:),allocatable :: b
         integer :: i = 0
         logical :: l = .false.
       end type

       contains

       subroutine init_TMP(this,that)
         implicit none
         type(tmp),intent(inout) :: this
         type(tmp),intent(in) :: that
         integer :: i_iter
         call delete(this)
         call init(this%g,that%g)
         if (allocated(that%b)) then
           allocate(this%b(size(that%b)))
           do i_iter=1,size(this%b)
             call init(this%b(i_iter),that%b(i_iter))
           enddo
         endif
         this%i = that%i
         this%l = that%l
       end subroutine

       subroutine init_many_TMP(this,that)
         implicit none
         type(tmp),dimension(:),intent(inout) :: this
         type(tmp),dimension(:),intent(in) :: that
         integer :: i_iter
         if (allocated(that)) then
           allocate(this(size(that)))
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_TMP(this)
         implicit none
         type(tmp),intent(inout) :: this
         integer :: i_iter
         call delete(this%g)
         if (allocated(this%b)) then
           do i_iter=1,size(this%b)
             call delete(this%b(i))
           enddo
           deallocate(this%b)
         endif
         this%i = 0
         this%l = .false.
       end subroutine

       subroutine delete_many_TMP(this)
         implicit none
         type(tmp),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
           deallocate(this)
         endif
       end subroutine

       subroutine display_TMP(this,un)
         implicit none
         type(tmp),intent(in) :: this
         integer,intent(in) :: un
         call display(this%g,un)
         call display(this%b,un)
         write(un,*) 'i = ',this%i
         write(un,*) 'l = ',this%l
       end subroutine

       subroutine display_many_TMP(this,un)
         implicit none
         type(tmp),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (allocated(this)) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_TMP(this)
         implicit none
         type(tmp),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_TMP(this)
         implicit none
         type(tmp),dimension(:),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_TMP(this,un)
         implicit none
         type(tmp),intent(in) :: this
         integer,intent(in) :: un
         call export(this%g,un)
         call export(this%b,un)
         write(un,*) this%i
         write(un,*) this%l
       end subroutine

       subroutine import_TMP(this,un)
         implicit none
         type(tmp),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%g,un)
         call import(this%b,un)
         read(un,*) this%i
         read(un,*) this%l
       end subroutine

       end module