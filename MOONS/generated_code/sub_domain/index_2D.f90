       module INDEX_2D_mod
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
       public :: INDEX_2D
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_index_2d;          end interface
       interface init;   module procedure init_many_index_2d;     end interface
       interface delete; module procedure delete_index_2d;        end interface
       interface delete; module procedure delete_many_index_2d;   end interface
       interface display;module procedure display_index_2d;       end interface
       interface display;module procedure display_many_index_2d;  end interface
       interface print;  module procedure print_index_2d;         end interface
       interface print;  module procedure print_many_index_2d;    end interface
       interface export; module procedure export_index_2d;        end interface
       interface export; module procedure export_many_index_2d;   end interface
       interface import; module procedure import_index_2d;        end interface
       interface import; module procedure import_many_index_2d;   end interface
       interface export; module procedure export_wrapper_index_2d;end interface
       interface import; module procedure import_wrapper_index_2d;end interface

       type INDEX_2D
         private
         integer,dimension(2) :: i = 0
       end type

       contains

       subroutine init_INDEX_2D(this,that)
         implicit none
         type(index_2d),intent(inout) :: this
         type(index_2d),intent(in) :: that
         call delete(this)
         this%i = that%i
       end subroutine

       subroutine init_many_INDEX_2D(this,that)
         implicit none
         type(index_2d),dimension(:),intent(inout) :: this
         type(index_2d),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_INDEX_2D(this)
         implicit none
         type(index_2d),intent(inout) :: this
         this%i = 0
       end subroutine

       subroutine delete_many_INDEX_2D(this)
         implicit none
         type(index_2d),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_INDEX_2D(this,un)
         implicit none
         type(index_2d),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i = ',this%i
       end subroutine

       subroutine display_many_INDEX_2D(this,un)
         implicit none
         type(index_2d),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_INDEX_2D(this)
         implicit none
         type(index_2d),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_INDEX_2D(this)
         implicit none
         type(index_2d),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_INDEX_2D(this,un)
         implicit none
         type(index_2d),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%i
       end subroutine

       subroutine export_many_INDEX_2D(this,un)
         implicit none
         type(index_2d),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_INDEX_2D(this,un)
         implicit none
         type(index_2d),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%i
       end subroutine

       subroutine import_many_INDEX_2D(this,un)
         implicit none
         type(index_2d),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_INDEX_2D(this,dir,name)
         implicit none
         type(index_2d),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_INDEX_2D(this,dir,name)
         implicit none
         type(index_2d),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module