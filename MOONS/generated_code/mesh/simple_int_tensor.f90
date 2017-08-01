       module SIMPLE_INT_TENSOR_mod
       use IO_tools_mod
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
       public :: SIMPLE_INT_TENSOR
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_simple_int_tensor;          end interface
       interface init;   module procedure init_many_simple_int_tensor;     end interface
       interface delete; module procedure delete_simple_int_tensor;        end interface
       interface delete; module procedure delete_many_simple_int_tensor;   end interface
       interface display;module procedure display_simple_int_tensor;       end interface
       interface display;module procedure display_many_simple_int_tensor;  end interface
       interface print;  module procedure print_simple_int_tensor;         end interface
       interface print;  module procedure print_many_simple_int_tensor;    end interface
       interface export; module procedure export_simple_int_tensor;        end interface
       interface export; module procedure export_many_simple_int_tensor;   end interface
       interface import; module procedure import_simple_int_tensor;        end interface
       interface import; module procedure import_many_simple_int_tensor;   end interface
       interface export; module procedure export_wrapper_simple_int_tensor;end interface
       interface import; module procedure import_wrapper_simple_int_tensor;end interface

       type SIMPLE_INT_TENSOR
         private
         integer,dimension(3) :: eye = 0
       end type

       contains

       subroutine init_SIMPLE_INT_TENSOR(this,that)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         type(simple_int_tensor),intent(in) :: that
         call delete(this)
         this%eye = that%eye
       end subroutine

       subroutine init_many_SIMPLE_INT_TENSOR(this,that)
         implicit none
         type(simple_int_tensor),dimension(:),intent(inout) :: this
         type(simple_int_tensor),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SIMPLE_INT_TENSOR(this)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         this%eye = 0
       end subroutine

       subroutine delete_many_SIMPLE_INT_TENSOR(this)
         implicit none
         type(simple_int_tensor),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'eye = ',this%eye
       end subroutine

       subroutine display_many_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SIMPLE_INT_TENSOR(this)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SIMPLE_INT_TENSOR(this)
         implicit none
         type(simple_int_tensor),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%eye
       end subroutine

       subroutine export_many_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%eye
       end subroutine

       subroutine import_many_SIMPLE_INT_TENSOR(this,un)
         implicit none
         type(simple_int_tensor),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SIMPLE_INT_TENSOR(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SIMPLE_INT_TENSOR(this,dir,name)
         implicit none
         type(simple_int_tensor),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module