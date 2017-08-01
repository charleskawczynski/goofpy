       module MOMENTUM_TERMS_mod
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
       public :: MOMENTUM_TERMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_momentum_terms;          end interface
       interface init;   module procedure init_many_momentum_terms;     end interface
       interface delete; module procedure delete_momentum_terms;        end interface
       interface delete; module procedure delete_many_momentum_terms;   end interface
       interface display;module procedure display_momentum_terms;       end interface
       interface display;module procedure display_many_momentum_terms;  end interface
       interface print;  module procedure print_momentum_terms;         end interface
       interface print;  module procedure print_many_momentum_terms;    end interface
       interface export; module procedure export_momentum_terms;        end interface
       interface export; module procedure export_many_momentum_terms;   end interface
       interface import; module procedure import_momentum_terms;        end interface
       interface import; module procedure import_many_momentum_terms;   end interface
       interface export; module procedure export_wrapper_momentum_terms;end interface
       interface import; module procedure import_wrapper_momentum_terms;end interface

       type MOMENTUM_TERMS
         private
         type(equation_term) :: pressure_grad
         type(equation_term) :: advection_divergence
         type(equation_term) :: advection_convection
         type(equation_term) :: advection_base_flow
         type(equation_term) :: diffusion
         type(equation_term) :: mean_pressure_grad
         type(equation_term) :: jcrossb
         type(equation_term) :: q2d_jcrossb
         type(equation_term) :: buoyancy
         type(equation_term) :: gravity
       end type

       contains

       subroutine init_MOMENTUM_TERMS(this,that)
         implicit none
         type(momentum_terms),intent(inout) :: this
         type(momentum_terms),intent(in) :: that
         call delete(this)
         call init(this%pressure_grad,that%pressure_grad)
         call init(this%advection_divergence,that%advection_divergence)
         call init(this%advection_convection,that%advection_convection)
         call init(this%advection_base_flow,that%advection_base_flow)
         call init(this%diffusion,that%diffusion)
         call init(this%mean_pressure_grad,that%mean_pressure_grad)
         call init(this%jcrossb,that%jcrossb)
         call init(this%q2d_jcrossb,that%q2d_jcrossb)
         call init(this%buoyancy,that%buoyancy)
         call init(this%gravity,that%gravity)
       end subroutine

       subroutine init_many_MOMENTUM_TERMS(this,that)
         implicit none
         type(momentum_terms),dimension(:),intent(inout) :: this
         type(momentum_terms),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MOMENTUM_TERMS(this)
         implicit none
         type(momentum_terms),intent(inout) :: this
         call delete(this%pressure_grad)
         call delete(this%advection_divergence)
         call delete(this%advection_convection)
         call delete(this%advection_base_flow)
         call delete(this%diffusion)
         call delete(this%mean_pressure_grad)
         call delete(this%jcrossb)
         call delete(this%q2d_jcrossb)
         call delete(this%buoyancy)
         call delete(this%gravity)
       end subroutine

       subroutine delete_many_MOMENTUM_TERMS(this)
         implicit none
         type(momentum_terms),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%pressure_grad,un)
         call display(this%advection_divergence,un)
         call display(this%advection_convection,un)
         call display(this%advection_base_flow,un)
         call display(this%diffusion,un)
         call display(this%mean_pressure_grad,un)
         call display(this%jcrossb,un)
         call display(this%q2d_jcrossb,un)
         call display(this%buoyancy,un)
         call display(this%gravity,un)
       end subroutine

       subroutine display_many_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MOMENTUM_TERMS(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MOMENTUM_TERMS(this)
         implicit none
         type(momentum_terms),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%pressure_grad,un)
         call export(this%advection_divergence,un)
         call export(this%advection_convection,un)
         call export(this%advection_base_flow,un)
         call export(this%diffusion,un)
         call export(this%mean_pressure_grad,un)
         call export(this%jcrossb,un)
         call export(this%q2d_jcrossb,un)
         call export(this%buoyancy,un)
         call export(this%gravity,un)
       end subroutine

       subroutine export_many_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%pressure_grad,un)
         call import(this%advection_divergence,un)
         call import(this%advection_convection,un)
         call import(this%advection_base_flow,un)
         call import(this%diffusion,un)
         call import(this%mean_pressure_grad,un)
         call import(this%jcrossb,un)
         call import(this%q2d_jcrossb,un)
         call import(this%buoyancy,un)
         call import(this%gravity,un)
       end subroutine

       subroutine import_many_MOMENTUM_TERMS(this,un)
         implicit none
         type(momentum_terms),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MOMENTUM_TERMS(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MOMENTUM_TERMS(this,dir,name)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module