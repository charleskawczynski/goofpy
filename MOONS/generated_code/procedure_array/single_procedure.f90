       module SINGLE_PROCEDURE_mod
       use IO_tools_mod
       use apply_face_BC_op_mod
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
       public :: SINGLE_PROCEDURE
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_single_procedure;          end interface
       interface init;   module procedure init_many_single_procedure;     end interface
       interface delete; module procedure delete_single_procedure;        end interface
       interface delete; module procedure delete_many_single_procedure;   end interface
       interface display;module procedure display_single_procedure;       end interface
       interface display;module procedure display_many_single_procedure;  end interface
       interface print;  module procedure print_single_procedure;         end interface
       interface print;  module procedure print_many_single_procedure;    end interface
       interface export; module procedure export_single_procedure;        end interface
       interface export; module procedure export_many_single_procedure;   end interface
       interface import; module procedure import_single_procedure;        end interface
       interface import; module procedure import_many_single_procedure;   end interface
       interface export; module procedure export_wrapper_single_procedure;end interface
       interface import; module procedure import_wrapper_single_procedure;end interface

       type SINGLE_PROCEDURE
         private
         procedure(apply_face_bc_op),pointer,nopass :: p
         logical :: defined = .false.
         integer :: id = 0
       end type

       contains

       subroutine init_SINGLE_PROCEDURE(this,that)
         implicit none
         type(single_procedure),intent(inout) :: this
         type(single_procedure),intent(in) :: that
         call delete(this)
         this%p => that%p
         this%defined = that%defined
         this%id = that%id
       end subroutine

       subroutine init_many_SINGLE_PROCEDURE(this,that)
         implicit none
         type(single_procedure),dimension(:),intent(inout) :: this
         type(single_procedure),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SINGLE_PROCEDURE(this)
         implicit none
         type(single_procedure),intent(inout) :: this
           nullify(this%p)
         this%defined = .false.
         this%id = 0
       end subroutine

       subroutine delete_many_SINGLE_PROCEDURE(this)
         implicit none
         type(single_procedure),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined = ',this%defined
         write(un,*) 'id      = ',this%id
       end subroutine

       subroutine display_many_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SINGLE_PROCEDURE(this)
         implicit none
         type(single_procedure),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SINGLE_PROCEDURE(this)
         implicit none
         type(single_procedure),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%defined
         write(un,*) this%id
       end subroutine

       subroutine export_many_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%defined
         read(un,*) this%id
       end subroutine

       subroutine import_many_SINGLE_PROCEDURE(this,un)
         implicit none
         type(single_procedure),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SINGLE_PROCEDURE(this,dir,name)
         implicit none
         type(single_procedure),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SINGLE_PROCEDURE(this,dir,name)
         implicit none
         type(single_procedure),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module