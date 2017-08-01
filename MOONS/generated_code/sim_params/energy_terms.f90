       module ENERGY_TERMS_mod
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
       public :: ENERGY_TERMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_energy_terms;          end interface
       interface init;   module procedure init_many_energy_terms;     end interface
       interface delete; module procedure delete_energy_terms;        end interface
       interface delete; module procedure delete_many_energy_terms;   end interface
       interface display;module procedure display_energy_terms;       end interface
       interface display;module procedure display_many_energy_terms;  end interface
       interface print;  module procedure print_energy_terms;         end interface
       interface print;  module procedure print_many_energy_terms;    end interface
       interface export; module procedure export_energy_terms;        end interface
       interface export; module procedure export_many_energy_terms;   end interface
       interface import; module procedure import_energy_terms;        end interface
       interface import; module procedure import_many_energy_terms;   end interface
       interface export; module procedure export_wrapper_energy_terms;end interface
       interface import; module procedure import_wrapper_energy_terms;end interface

       type ENERGY_TERMS
         private
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: ke_diffusion
         type(equation_term) :: viscous_dissipation
         type(equation_term) :: joule_heating
         type(equation_term) :: volumetric_heating
       end type

       contains

       subroutine init_ENERGY_TERMS(this,that)
         implicit none
         type(energy_terms),intent(inout) :: this
         type(energy_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%ke_diffusion,that%ke_diffusion)
         call init(this%viscous_dissipation,that%viscous_dissipation)
         call init(this%joule_heating,that%joule_heating)
         call init(this%volumetric_heating,that%volumetric_heating)
       end subroutine

       subroutine init_many_ENERGY_TERMS(this,that)
         implicit none
         type(energy_terms),dimension(:),intent(inout) :: this
         type(energy_terms),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_ENERGY_TERMS(this)
         implicit none
         type(energy_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%ke_diffusion)
         call delete(this%viscous_dissipation)
         call delete(this%joule_heating)
         call delete(this%volumetric_heating)
       end subroutine

       subroutine delete_many_ENERGY_TERMS(this)
         implicit none
         type(energy_terms),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%ke_diffusion,un)
         call display(this%viscous_dissipation,un)
         call display(this%joule_heating,un)
         call display(this%volumetric_heating,un)
       end subroutine

       subroutine display_many_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_ENERGY_TERMS(this)
         implicit none
         type(energy_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_ENERGY_TERMS(this)
         implicit none
         type(energy_terms),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%ke_diffusion,un)
         call export(this%viscous_dissipation,un)
         call export(this%joule_heating,un)
         call export(this%volumetric_heating,un)
       end subroutine

       subroutine export_many_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%ke_diffusion,un)
         call import(this%viscous_dissipation,un)
         call import(this%joule_heating,un)
         call import(this%volumetric_heating,un)
       end subroutine

       subroutine import_many_ENERGY_TERMS(this,un)
         implicit none
         type(energy_terms),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_ENERGY_TERMS(this,dir,name)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_ENERGY_TERMS(this,dir,name)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module