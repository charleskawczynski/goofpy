       module ITER_SOLVER_PARAMS_mod
       use IO_tools_mod
       use string_mod
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
       public :: ITER_SOLVER_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_iter_solver_params;          end interface
       interface init;   module procedure init_many_iter_solver_params;     end interface
       interface delete; module procedure delete_iter_solver_params;        end interface
       interface delete; module procedure delete_many_iter_solver_params;   end interface
       interface display;module procedure display_iter_solver_params;       end interface
       interface display;module procedure display_many_iter_solver_params;  end interface
       interface print;  module procedure print_iter_solver_params;         end interface
       interface print;  module procedure print_many_iter_solver_params;    end interface
       interface export; module procedure export_iter_solver_params;        end interface
       interface export; module procedure export_many_iter_solver_params;   end interface
       interface import; module procedure import_iter_solver_params;        end interface
       interface import; module procedure import_many_iter_solver_params;   end interface
       interface export; module procedure export_wrapper_iter_solver_params;end interface
       interface import; module procedure import_wrapper_iter_solver_params;end interface

       type ITER_SOLVER_PARAMS
         private
         integer :: un = 0
         type(string) :: dir
         type(string) :: name
         integer :: iter_max = 0
         real(cp) :: tol_abs = 0.0_cp
         real(cp) :: tol_rel = 0.0_cp
         integer :: iter_total = 0
         integer :: iter_per_call = 0
         integer :: n_skip_check_res = 0
         logical :: export_convergence = .false.
         logical :: export_heavy = .false.
         logical,dimension(3) :: exit_loop = .false.
       end type

       contains

       subroutine init_ITER_SOLVER_PARAMS(this,that)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         type(iter_solver_params),intent(in) :: that
         call delete(this)
         this%un = that%un
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%iter_max = that%iter_max
         this%tol_abs = that%tol_abs
         this%tol_rel = that%tol_rel
         this%iter_total = that%iter_total
         this%iter_per_call = that%iter_per_call
         this%n_skip_check_res = that%n_skip_check_res
         this%export_convergence = that%export_convergence
         this%export_heavy = that%export_heavy
         this%exit_loop = that%exit_loop
       end subroutine

       subroutine init_many_ITER_SOLVER_PARAMS(this,that)
         implicit none
         type(iter_solver_params),dimension(:),intent(inout) :: this
         type(iter_solver_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_ITER_SOLVER_PARAMS(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         this%un = 0
         call delete(this%dir)
         call delete(this%name)
         this%iter_max = 0
         this%tol_abs = 0.0_cp
         this%tol_rel = 0.0_cp
         this%iter_total = 0
         this%iter_per_call = 0
         this%n_skip_check_res = 0
         this%export_convergence = .false.
         this%export_heavy = .false.
         this%exit_loop = .false.
       end subroutine

       subroutine delete_many_ITER_SOLVER_PARAMS(this)
         implicit none
         type(iter_solver_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un                 = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_max           = ',this%iter_max
         write(un,*) 'tol_abs            = ',this%tol_abs
         write(un,*) 'tol_rel            = ',this%tol_rel
         write(un,*) 'iter_total         = ',this%iter_total
         write(un,*) 'iter_per_call      = ',this%iter_per_call
         write(un,*) 'n_skip_check_res   = ',this%n_skip_check_res
         write(un,*) 'export_convergence = ',this%export_convergence
         write(un,*) 'export_heavy       = ',this%export_heavy
         write(un,*) 'exit_loop          = ',this%exit_loop
       end subroutine

       subroutine display_many_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_ITER_SOLVER_PARAMS(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_ITER_SOLVER_PARAMS(this)
         implicit none
         type(iter_solver_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%un
         call export(this%dir,un)
         call export(this%name,un)
         write(un,*) this%iter_max
         write(un,*) this%tol_abs
         write(un,*) this%tol_rel
         write(un,*) this%iter_total
         write(un,*) this%iter_per_call
         write(un,*) this%n_skip_check_res
         write(un,*) this%export_convergence
         write(un,*) this%export_heavy
         write(un,*) this%exit_loop
       end subroutine

       subroutine export_many_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%un
         call import(this%dir,un)
         call import(this%name,un)
         read(un,*) this%iter_max
         read(un,*) this%tol_abs
         read(un,*) this%tol_rel
         read(un,*) this%iter_total
         read(un,*) this%iter_per_call
         read(un,*) this%n_skip_check_res
         read(un,*) this%export_convergence
         read(un,*) this%export_heavy
         read(un,*) this%exit_loop
       end subroutine

       subroutine import_many_ITER_SOLVER_PARAMS(this,un)
         implicit none
         type(iter_solver_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_ITER_SOLVER_PARAMS(this,dir,name)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_ITER_SOLVER_PARAMS(this,dir,name)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module