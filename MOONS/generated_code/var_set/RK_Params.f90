       module RK_PARAMS_mod
       use IO_tools_mod
       use array_mod
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
       public :: RK_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_rk_params;          end interface
       interface init;   module procedure init_many_rk_params;     end interface
       interface delete; module procedure delete_rk_params;        end interface
       interface delete; module procedure delete_many_rk_params;   end interface
       interface display;module procedure display_rk_params;       end interface
       interface display;module procedure display_many_rk_params;  end interface
       interface print;  module procedure print_rk_params;         end interface
       interface print;  module procedure print_many_rk_params;    end interface
       interface export; module procedure export_rk_params;        end interface
       interface export; module procedure export_many_rk_params;   end interface
       interface import; module procedure import_rk_params;        end interface
       interface import; module procedure import_many_rk_params;   end interface
       interface export; module procedure export_wrapper_rk_params;end interface
       interface import; module procedure import_wrapper_rk_params;end interface

       type RK_PARAMS
         private
         integer :: n_stages = 0
         integer :: n = 0
         logical :: rk_active = .false.
         type(array) :: gamma
         type(array) :: zeta
         type(array) :: alpha
         type(array) :: beta
       end type

       contains

       subroutine init_RK_PARAMS(this,that)
         implicit none
         type(rk_params),intent(inout) :: this
         type(rk_params),intent(in) :: that
         call delete(this)
         this%n_stages = that%n_stages
         this%n = that%n
         this%rk_active = that%rk_active
         call init(this%gamma,that%gamma)
         call init(this%zeta,that%zeta)
         call init(this%alpha,that%alpha)
         call init(this%beta,that%beta)
       end subroutine

       subroutine init_many_RK_PARAMS(this,that)
         implicit none
         type(rk_params),dimension(:),intent(inout) :: this
         type(rk_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_RK_PARAMS(this)
         implicit none
         type(rk_params),intent(inout) :: this
         this%n_stages = 0
         this%n = 0
         this%rk_active = .false.
         call delete(this%gamma)
         call delete(this%zeta)
         call delete(this%alpha)
         call delete(this%beta)
       end subroutine

       subroutine delete_many_RK_PARAMS(this)
         implicit none
         type(rk_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_RK_PARAMS(this,un)
         implicit none
         type(rk_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages  = ',this%n_stages
         write(un,*) 'n         = ',this%n
         write(un,*) 'rk_active = ',this%rk_active
         call display(this%gamma,un)
         call display(this%zeta,un)
         call display(this%alpha,un)
         call display(this%beta,un)
       end subroutine

       subroutine display_many_RK_PARAMS(this,un)
         implicit none
         type(rk_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_RK_PARAMS(this)
         implicit none
         type(rk_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_RK_PARAMS(this)
         implicit none
         type(rk_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_RK_PARAMS(this,un)
         implicit none
         type(rk_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%n_stages
         write(un,*) this%n
         write(un,*) this%rk_active
         call export(this%gamma,un)
         call export(this%zeta,un)
         call export(this%alpha,un)
         call export(this%beta,un)
       end subroutine

       subroutine export_many_RK_PARAMS(this,un)
         implicit none
         type(rk_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_RK_PARAMS(this,un)
         implicit none
         type(rk_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%n_stages
         read(un,*) this%n
         read(un,*) this%rk_active
         call import(this%gamma,un)
         call import(this%zeta,un)
         call import(this%alpha,un)
         call import(this%beta,un)
       end subroutine

       subroutine import_many_RK_PARAMS(this,un)
         implicit none
         type(rk_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_RK_PARAMS(this,dir,name)
         implicit none
         type(rk_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_RK_PARAMS(this,dir,name)
         implicit none
         type(rk_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module