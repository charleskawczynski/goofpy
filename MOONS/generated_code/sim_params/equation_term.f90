       module EQUATION_TERM_mod
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
       public :: EQUATION_TERM
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_equation_term;          end interface
       interface init;   module procedure init_many_equation_term;     end interface
       interface delete; module procedure delete_equation_term;        end interface
       interface delete; module procedure delete_many_equation_term;   end interface
       interface display;module procedure display_equation_term;       end interface
       interface display;module procedure display_many_equation_term;  end interface
       interface print;  module procedure print_equation_term;         end interface
       interface print;  module procedure print_many_equation_term;    end interface
       interface export; module procedure export_equation_term;        end interface
       interface export; module procedure export_many_equation_term;   end interface
       interface import; module procedure import_equation_term;        end interface
       interface import; module procedure import_many_equation_term;   end interface
       interface export; module procedure export_wrapper_equation_term;end interface
       interface import; module procedure import_wrapper_equation_term;end interface

       type EQUATION_TERM
         private
         logical :: add = .false.
         real(cp) :: scale = 0.0_cp
       end type

       contains

       subroutine init_EQUATION_TERM(this,that)
         implicit none
         type(equation_term),intent(inout) :: this
         type(equation_term),intent(in) :: that
         call delete(this)
         this%add = that%add
         this%scale = that%scale
       end subroutine

       subroutine init_many_EQUATION_TERM(this,that)
         implicit none
         type(equation_term),dimension(:),intent(inout) :: this
         type(equation_term),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EQUATION_TERM(this)
         implicit none
         type(equation_term),intent(inout) :: this
         this%add = .false.
         this%scale = 0.0_cp
       end subroutine

       subroutine delete_many_EQUATION_TERM(this)
         implicit none
         type(equation_term),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'add   = ',this%add
         write(un,*) 'scale = ',this%scale
       end subroutine

       subroutine display_many_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EQUATION_TERM(this)
         implicit none
         type(equation_term),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EQUATION_TERM(this)
         implicit none
         type(equation_term),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%add
         write(un,*) this%scale
       end subroutine

       subroutine export_many_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%add
         read(un,*) this%scale
       end subroutine

       subroutine import_many_EQUATION_TERM(this,un)
         implicit none
         type(equation_term),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EQUATION_TERM(this,dir,name)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EQUATION_TERM(this,dir,name)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module