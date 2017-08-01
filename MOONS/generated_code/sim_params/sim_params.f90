       module SIM_PARAMS_mod
       use IO_tools_mod
       use export_logicals_mod
       use dimensionless_params_mod
       use flow_control_logicals_mod
       use var_set_mod
       use momentum_terms_mod
       use induction_terms_mod
       use geometry_props_mod
       use energy_terms_mod
       use mesh_quality_params_mod
       use mirror_props_mod
       use export_frequency_mod
       use time_marching_params_mod
       use time_statistics_params_mod
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
       public :: SIM_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_sim_params;          end interface
       interface init;   module procedure init_many_sim_params;     end interface
       interface delete; module procedure delete_sim_params;        end interface
       interface delete; module procedure delete_many_sim_params;   end interface
       interface display;module procedure display_sim_params;       end interface
       interface display;module procedure display_many_sim_params;  end interface
       interface print;  module procedure print_sim_params;         end interface
       interface print;  module procedure print_many_sim_params;    end interface
       interface export; module procedure export_sim_params;        end interface
       interface export; module procedure export_many_sim_params;   end interface
       interface import; module procedure import_sim_params;        end interface
       interface import; module procedure import_many_sim_params;   end interface
       interface export; module procedure export_wrapper_sim_params;end interface
       interface import; module procedure import_wrapper_sim_params;end interface

       type SIM_PARAMS
         private
         type(var_set) :: vs
         type(mesh_quality_params) :: mqp
         type(dimensionless_params) :: dp
         type(export_logicals) :: el
         type(export_frequency) :: ef
         type(energy_terms) :: et
         type(momentum_terms) :: mt
         type(induction_terms) :: it
         type(geometry_props) :: gp
         type(mirror_props) :: mp
         type(time_marching_params) :: coupled
         type(flow_control_logicals) :: fcl
         type(time_statistics_params) :: tsp
         real(cp) :: export_safe_period = 0.0_cp
         logical :: restart_meshes = .false.
         logical :: export_heavy = .false.
         logical :: matrix_based = .false.
         logical :: print_every_mhd_step = .false.
         logical :: couple_time_steps = .false.
         logical :: finite_rem = .false.
         logical :: include_vacuum = .false.
         logical :: embed_b_interior = .false.
         logical :: compute_surface_power = .false.
         integer :: uniform_b0_dir = 0
         integer :: mpg_dir = 0
         integer :: uniform_gravity_dir = 0
       end type

       contains

       subroutine init_SIM_PARAMS(this,that)
         implicit none
         type(sim_params),intent(inout) :: this
         type(sim_params),intent(in) :: that
         call delete(this)
         call init(this%vs,that%vs)
         call init(this%mqp,that%mqp)
         call init(this%dp,that%dp)
         call init(this%el,that%el)
         call init(this%ef,that%ef)
         call init(this%et,that%et)
         call init(this%mt,that%mt)
         call init(this%it,that%it)
         call init(this%gp,that%gp)
         call init(this%mp,that%mp)
         call init(this%coupled,that%coupled)
         call init(this%fcl,that%fcl)
         call init(this%tsp,that%tsp)
         this%export_safe_period = that%export_safe_period
         this%restart_meshes = that%restart_meshes
         this%export_heavy = that%export_heavy
         this%matrix_based = that%matrix_based
         this%print_every_mhd_step = that%print_every_mhd_step
         this%couple_time_steps = that%couple_time_steps
         this%finite_rem = that%finite_rem
         this%include_vacuum = that%include_vacuum
         this%embed_b_interior = that%embed_b_interior
         this%compute_surface_power = that%compute_surface_power
         this%uniform_b0_dir = that%uniform_b0_dir
         this%mpg_dir = that%mpg_dir
         this%uniform_gravity_dir = that%uniform_gravity_dir
       end subroutine

       subroutine init_many_SIM_PARAMS(this,that)
         implicit none
         type(sim_params),dimension(:),intent(inout) :: this
         type(sim_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SIM_PARAMS(this)
         implicit none
         type(sim_params),intent(inout) :: this
         call delete(this%vs)
         call delete(this%mqp)
         call delete(this%dp)
         call delete(this%el)
         call delete(this%ef)
         call delete(this%et)
         call delete(this%mt)
         call delete(this%it)
         call delete(this%gp)
         call delete(this%mp)
         call delete(this%coupled)
         call delete(this%fcl)
         call delete(this%tsp)
         this%export_safe_period = 0.0_cp
         this%restart_meshes = .false.
         this%export_heavy = .false.
         this%matrix_based = .false.
         this%print_every_mhd_step = .false.
         this%couple_time_steps = .false.
         this%finite_rem = .false.
         this%include_vacuum = .false.
         this%embed_b_interior = .false.
         this%compute_surface_power = .false.
         this%uniform_b0_dir = 0
         this%mpg_dir = 0
         this%uniform_gravity_dir = 0
       end subroutine

       subroutine delete_many_SIM_PARAMS(this)
         implicit none
         type(sim_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%vs,un)
         call display(this%mqp,un)
         call display(this%dp,un)
         call display(this%el,un)
         call display(this%ef,un)
         call display(this%et,un)
         call display(this%mt,un)
         call display(this%it,un)
         call display(this%gp,un)
         call display(this%mp,un)
         call display(this%coupled,un)
         call display(this%fcl,un)
         call display(this%tsp,un)
         write(un,*) 'export_safe_period    = ',this%export_safe_period
         write(un,*) 'restart_meshes        = ',this%restart_meshes
         write(un,*) 'export_heavy          = ',this%export_heavy
         write(un,*) 'matrix_based          = ',this%matrix_based
         write(un,*) 'print_every_mhd_step  = ',this%print_every_mhd_step
         write(un,*) 'couple_time_steps     = ',this%couple_time_steps
         write(un,*) 'finite_rem            = ',this%finite_rem
         write(un,*) 'include_vacuum        = ',this%include_vacuum
         write(un,*) 'embed_b_interior      = ',this%embed_b_interior
         write(un,*) 'compute_surface_power = ',this%compute_surface_power
         write(un,*) 'uniform_b0_dir        = ',this%uniform_b0_dir
         write(un,*) 'mpg_dir               = ',this%mpg_dir
         write(un,*) 'uniform_gravity_dir   = ',this%uniform_gravity_dir
       end subroutine

       subroutine display_many_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SIM_PARAMS(this)
         implicit none
         type(sim_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SIM_PARAMS(this)
         implicit none
         type(sim_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),intent(in) :: this
         integer,intent(in) :: un
         call export(this%vs,un)
         call export(this%mqp,un)
         call export(this%dp,un)
         call export(this%el,un)
         call export(this%ef,un)
         call export(this%et,un)
         call export(this%mt,un)
         call export(this%it,un)
         call export(this%gp,un)
         call export(this%mp,un)
         call export(this%coupled,un)
         call export(this%fcl,un)
         call export(this%tsp,un)
         write(un,*) this%export_safe_period
         write(un,*) this%restart_meshes
         write(un,*) this%export_heavy
         write(un,*) this%matrix_based
         write(un,*) this%print_every_mhd_step
         write(un,*) this%couple_time_steps
         write(un,*) this%finite_rem
         write(un,*) this%include_vacuum
         write(un,*) this%embed_b_interior
         write(un,*) this%compute_surface_power
         write(un,*) this%uniform_b0_dir
         write(un,*) this%mpg_dir
         write(un,*) this%uniform_gravity_dir
       end subroutine

       subroutine export_many_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%vs,un)
         call import(this%mqp,un)
         call import(this%dp,un)
         call import(this%el,un)
         call import(this%ef,un)
         call import(this%et,un)
         call import(this%mt,un)
         call import(this%it,un)
         call import(this%gp,un)
         call import(this%mp,un)
         call import(this%coupled,un)
         call import(this%fcl,un)
         call import(this%tsp,un)
         read(un,*) this%export_safe_period
         read(un,*) this%restart_meshes
         read(un,*) this%export_heavy
         read(un,*) this%matrix_based
         read(un,*) this%print_every_mhd_step
         read(un,*) this%couple_time_steps
         read(un,*) this%finite_rem
         read(un,*) this%include_vacuum
         read(un,*) this%embed_b_interior
         read(un,*) this%compute_surface_power
         read(un,*) this%uniform_b0_dir
         read(un,*) this%mpg_dir
         read(un,*) this%uniform_gravity_dir
       end subroutine

       subroutine import_many_SIM_PARAMS(this,un)
         implicit none
         type(sim_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SIM_PARAMS(this,dir,name)
         implicit none
         type(sim_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SIM_PARAMS(this,dir,name)
         implicit none
         type(sim_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module