       module SOLVER_SETTINGS_mod
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
       public :: SOLVER_SETTINGS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_solver_settings;          end interface
       interface init;   module procedure init_many_solver_settings;     end interface
       interface delete; module procedure delete_solver_settings;        end interface
       interface delete; module procedure delete_many_solver_settings;   end interface
       interface display;module procedure display_solver_settings;       end interface
       interface display;module procedure display_many_solver_settings;  end interface
       interface print;  module procedure print_solver_settings;         end interface
       interface print;  module procedure print_many_solver_settings;    end interface
       interface export; module procedure export_solver_settings;        end interface
       interface export; module procedure export_many_solver_settings;   end interface
       interface import; module procedure import_solver_settings;        end interface
       interface import; module procedure import_many_solver_settings;   end interface
       interface export; module procedure export_wrapper_solver_settings;end interface
       interface import; module procedure import_wrapper_solver_settings;end interface

       type SOLVER_SETTINGS
         private
         integer :: solve_method = 0
         logical :: initialize = .false.
         logical :: solve = .false.
         logical :: restart = .false.
         logical :: prescribed_bcs = .false.
       end type

       contains

       subroutine init_SOLVER_SETTINGS(this,that)
         implicit none
         type(solver_settings),intent(inout) :: this
         type(solver_settings),intent(in) :: that
         call delete(this)
         this%solve_method = that%solve_method
         this%initialize = that%initialize
         this%solve = that%solve
         this%restart = that%restart
         this%prescribed_bcs = that%prescribed_bcs
       end subroutine

       subroutine init_many_SOLVER_SETTINGS(this,that)
         implicit none
         type(solver_settings),dimension(:),intent(inout) :: this
         type(solver_settings),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SOLVER_SETTINGS(this)
         implicit none
         type(solver_settings),intent(inout) :: this
         this%solve_method = 0
         this%initialize = .false.
         this%solve = .false.
         this%restart = .false.
         this%prescribed_bcs = .false.
       end subroutine

       subroutine delete_many_SOLVER_SETTINGS(this)
         implicit none
         type(solver_settings),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method   = ',this%solve_method
         write(un,*) 'initialize     = ',this%initialize
         write(un,*) 'solve          = ',this%solve
         write(un,*) 'restart        = ',this%restart
         write(un,*) 'prescribed_bcs = ',this%prescribed_bcs
       end subroutine

       subroutine display_many_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SOLVER_SETTINGS(this)
         implicit none
         type(solver_settings),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SOLVER_SETTINGS(this)
         implicit none
         type(solver_settings),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%solve_method
         write(un,*) this%initialize
         write(un,*) this%solve
         write(un,*) this%restart
         write(un,*) this%prescribed_bcs
       end subroutine

       subroutine export_many_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%solve_method
         read(un,*) this%initialize
         read(un,*) this%solve
         read(un,*) this%restart
         read(un,*) this%prescribed_bcs
       end subroutine

       subroutine import_many_SOLVER_SETTINGS(this,un)
         implicit none
         type(solver_settings),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SOLVER_SETTINGS(this,dir,name)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SOLVER_SETTINGS(this,dir,name)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module