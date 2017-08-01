       module INDUCTION_TERMS_mod
       use IO_tools_mod
       use equation_term_mod
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
       public :: INDUCTION_TERMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_induction_terms;          end interface
       interface init;   module procedure init_many_induction_terms;     end interface
       interface delete; module procedure delete_induction_terms;        end interface
       interface delete; module procedure delete_many_induction_terms;   end interface
       interface display;module procedure display_induction_terms;       end interface
       interface display;module procedure display_many_induction_terms;  end interface
       interface print;  module procedure print_induction_terms;         end interface
       interface print;  module procedure print_many_induction_terms;    end interface
       interface export; module procedure export_induction_terms;        end interface
       interface export; module procedure export_many_induction_terms;   end interface
       interface import; module procedure import_induction_terms;        end interface
       interface import; module procedure import_many_induction_terms;   end interface
       interface export; module procedure export_wrapper_induction_terms;end interface
       interface import; module procedure import_wrapper_induction_terms;end interface

       type INDUCTION_TERMS
         private
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: unsteady_b0
         type(equation_term) :: current
         type(equation_term) :: b_applied
       end type

       contains

       subroutine init_INDUCTION_TERMS(this,that)
         implicit none
         type(induction_terms),intent(inout) :: this
         type(induction_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%unsteady_b0,that%unsteady_b0)
         call init(this%current,that%current)
         call init(this%b_applied,that%b_applied)
       end subroutine

       subroutine init_many_INDUCTION_TERMS(this,that)
         implicit none
         type(induction_terms),dimension(:),intent(inout) :: this
         type(induction_terms),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_INDUCTION_TERMS(this)
         implicit none
         type(induction_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%unsteady_b0)
         call delete(this%current)
         call delete(this%b_applied)
       end subroutine

       subroutine delete_many_INDUCTION_TERMS(this)
         implicit none
         type(induction_terms),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%unsteady_b0,un)
         call display(this%current,un)
         call display(this%b_applied,un)
       end subroutine

       subroutine display_many_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_INDUCTION_TERMS(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_INDUCTION_TERMS(this)
         implicit none
         type(induction_terms),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%unsteady_b0,un)
         call export(this%current,un)
         call export(this%b_applied,un)
       end subroutine

       subroutine export_many_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%unsteady_b0,un)
         call import(this%current,un)
         call import(this%b_applied,un)
       end subroutine

       subroutine import_many_INDUCTION_TERMS(this,un)
         implicit none
         type(induction_terms),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_INDUCTION_TERMS(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_INDUCTION_TERMS(this,dir,name)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module