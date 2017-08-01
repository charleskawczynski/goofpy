       module FLOW_CONTROL_LOGICALS_mod
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
       public :: FLOW_CONTROL_LOGICALS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_flow_control_logicals;          end interface
       interface init;   module procedure init_many_flow_control_logicals;     end interface
       interface delete; module procedure delete_flow_control_logicals;        end interface
       interface delete; module procedure delete_many_flow_control_logicals;   end interface
       interface display;module procedure display_flow_control_logicals;       end interface
       interface display;module procedure display_many_flow_control_logicals;  end interface
       interface print;  module procedure print_flow_control_logicals;         end interface
       interface print;  module procedure print_many_flow_control_logicals;    end interface
       interface export; module procedure export_flow_control_logicals;        end interface
       interface export; module procedure export_many_flow_control_logicals;   end interface
       interface import; module procedure import_flow_control_logicals;        end interface
       interface import; module procedure import_many_flow_control_logicals;   end interface
       interface export; module procedure export_wrapper_flow_control_logicals;end interface
       interface import; module procedure import_wrapper_flow_control_logicals;end interface

       type FLOW_CONTROL_LOGICALS
         private
         logical :: post_process = .false.
         logical :: skip_solver_loop = .false.
         logical :: stop_before_solve = .false.
         logical :: stop_after_mesh_export = .false.
         logical :: poisson_test = .false.
         logical :: taylor_green_vortex_test = .false.
       end type

       contains

       subroutine init_FLOW_CONTROL_LOGICALS(this,that)
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

       subroutine init_many_FLOW_CONTROL_LOGICALS(this,that)
         implicit none
         type(flow_control_logicals),dimension(:),intent(inout) :: this
         type(flow_control_logicals),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_FLOW_CONTROL_LOGICALS(this)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         this%post_process = .false.
         this%skip_solver_loop = .false.
         this%stop_before_solve = .false.
         this%stop_after_mesh_export = .false.
         this%poisson_test = .false.
         this%taylor_green_vortex_test = .false.
       end subroutine

       subroutine delete_many_FLOW_CONTROL_LOGICALS(this)
         implicit none
         type(flow_control_logicals),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_FLOW_CONTROL_LOGICALS(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'post_process             = ',this%post_process
         write(un,*) 'skip_solver_loop         = ',this%skip_solver_loop
         write(un,*) 'stop_before_solve        = ',this%stop_before_solve
         write(un,*) 'stop_after_mesh_export   = ',this%stop_after_mesh_export
         write(un,*) 'poisson_test             = ',this%poisson_test
         write(un,*) 'taylor_green_vortex_test = ',this%taylor_green_vortex_test
       end subroutine

       subroutine display_many_FLOW_CONTROL_LOGICALS(this,un)
         implicit none
         type(flow_control_logicals),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_FLOW_CONTROL_LOGICALS(this)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_FLOW_CONTROL_LOGICALS(this)
         implicit none
         type(flow_control_logicals),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_FLOW_CONTROL_LOGICALS(this,un)
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

       subroutine export_many_FLOW_CONTROL_LOGICALS(this,un)
         implicit none
         type(flow_control_logicals),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_FLOW_CONTROL_LOGICALS(this,un)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%post_process
         read(un,*) this%skip_solver_loop
         read(un,*) this%stop_before_solve
         read(un,*) this%stop_after_mesh_export
         read(un,*) this%poisson_test
         read(un,*) this%taylor_green_vortex_test
       end subroutine

       subroutine import_many_FLOW_CONTROL_LOGICALS(this,un)
         implicit none
         type(flow_control_logicals),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_FLOW_CONTROL_LOGICALS(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_FLOW_CONTROL_LOGICALS(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module