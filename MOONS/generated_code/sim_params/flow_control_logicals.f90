       module flow_control_logicals_mod
       use IO_tools_mod
       implicit none

       private
       public :: flow_control_logicals
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_flow_control_logicals;          end interface
       interface delete; module procedure delete_flow_control_logicals;        end interface
       interface display;module procedure display_flow_control_logicals;       end interface
       interface print;  module procedure print_flow_control_logicals;         end interface
       interface export; module procedure export_flow_control_logicals;        end interface
       interface import; module procedure import_flow_control_logicals;        end interface
       interface export; module procedure export_wrapper_flow_control_logicals;end interface
       interface import; module procedure import_wrapper_flow_control_logicals;end interface

       type flow_control_logicals
         logical :: post_process = .false.
         logical :: skip_solver_loop = .false.
         logical :: stop_before_solve = .false.
         logical :: stop_after_mesh_export = .false.
         logical :: poisson_test = .false.
         logical :: taylor_green_vortex_test = .false.
       end type

       contains

       subroutine init_flow_control_logicals(this,that)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         type(flow_control_logicals),intent(in) :: that
         call delete(this)
         this%post_process = that%post_process
         this%skip_solver_loop = that%skip_solver_loop
         this%stop_before_solve = that%stop_before_solve
         this%stop_after_mesh_export = that%stop_after_mesh_export
         this%poisson_test = that%poisson_test
         this%taylor_green_vortex_test = that%taylor_green_vortex_test
       end subroutine

       subroutine delete_flow_control_logicals(this)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         this%post_process = .false.
         this%skip_solver_loop = .false.
         this%stop_before_solve = .false.
         this%stop_after_mesh_export = .false.
         this%poisson_test = .false.
         this%taylor_green_vortex_test = .false.
       end subroutine

       subroutine display_flow_control_logicals(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- flow_control_logicals'
         write(un,*) 'post_process             = ',this%post_process
         write(un,*) 'skip_solver_loop         = ',this%skip_solver_loop
         write(un,*) 'stop_before_solve        = ',this%stop_before_solve
         write(un,*) 'stop_after_mesh_export   = ',this%stop_after_mesh_export
         write(un,*) 'poisson_test             = ',this%poisson_test
         write(un,*) 'taylor_green_vortex_test = ',this%taylor_green_vortex_test
       end subroutine

       subroutine print_flow_control_logicals(this)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_flow_control_logicals(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%post_process
         write(un,*) this%skip_solver_loop
         write(un,*) this%stop_before_solve
         write(un,*) this%stop_after_mesh_export
         write(un,*) this%poisson_test
         write(un,*) this%taylor_green_vortex_test
       end subroutine

       subroutine import_flow_control_logicals(this,un)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*) this%post_process
         read(un,*) this%skip_solver_loop
         read(un,*) this%stop_before_solve
         read(un,*) this%stop_after_mesh_export
         read(un,*) this%poisson_test
         read(un,*) this%taylor_green_vortex_test
       end subroutine

       subroutine export_wrapper_flow_control_logicals(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_flow_control_logicals(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module