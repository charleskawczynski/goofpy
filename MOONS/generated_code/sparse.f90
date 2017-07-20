       module SPARSE_mod
       use IO_tools_mod
       use array_mod
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
       public :: SPARSE
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_sparse;          end interface
       interface init;   module procedure init_many_sparse;     end interface
       interface delete; module procedure delete_sparse;        end interface
       interface delete; module procedure delete_many_sparse;   end interface
       interface display;module procedure display_sparse;       end interface
       interface display;module procedure display_many_sparse;  end interface
       interface print;  module procedure print_sparse;         end interface
       interface print;  module procedure print_many_sparse;    end interface
       interface export; module procedure export_sparse;        end interface
       interface export; module procedure export_many_sparse;   end interface
       interface import; module procedure import_sparse;        end interface
       interface import; module procedure import_many_sparse;   end interface
       interface export; module procedure export_wrapper_sparse;end interface
       interface import; module procedure import_wrapper_sparse;end interface

       type SPARSE
         type(array) :: l
         type(array) :: d
         type(array) :: u
         logical :: staggered = .false.
       end type

       contains

       subroutine init_SPARSE(this,that)
         implicit none
         type(sparse),intent(inout) :: this
         type(sparse),intent(in) :: that
         call delete(this)
         call init(this%l,that%l)
         call init(this%d,that%d)
         call init(this%u,that%u)
         this%staggered = that%staggered
       end subroutine

       subroutine init_many_SPARSE(this,that)
         implicit none
         type(sparse),dimension(:),intent(inout) :: this
         type(sparse),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SPARSE(this)
         implicit none
         type(sparse),intent(inout) :: this
         call delete(this%l)
         call delete(this%d)
         call delete(this%u)
         this%staggered = .false.
       end subroutine

       subroutine delete_many_SPARSE(this)
         implicit none
         type(sparse),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SPARSE(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call display(this%l,un)
         call display(this%d,un)
         call display(this%u,un)
         write(un,*) 'staggered = ',this%staggered
       end subroutine

       subroutine display_many_SPARSE(this,un)
         implicit none
         type(sparse),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SPARSE(this)
         implicit none
         type(sparse),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SPARSE(this)
         implicit none
         type(sparse),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SPARSE(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call export(this%l,un)
         call export(this%d,un)
         call export(this%u,un)
         write(un,*) this%staggered
       end subroutine

       subroutine export_many_SPARSE(this,un)
         implicit none
         type(sparse),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SPARSE(this,un)
         implicit none
         type(sparse),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%l,un)
         call import(this%d,un)
         call import(this%u,un)
         read(un,*) this%staggered
       end subroutine

       subroutine import_many_SPARSE(this,un)
         implicit none
         type(sparse),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SPARSE(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SPARSE(this,dir,name)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module