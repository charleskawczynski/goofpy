       module MATRIX_FREE_PARAMS_mod
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
       public :: MATRIX_FREE_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_matrix_free_params;          end interface
       interface init;   module procedure init_many_matrix_free_params;     end interface
       interface delete; module procedure delete_matrix_free_params;        end interface
       interface delete; module procedure delete_many_matrix_free_params;   end interface
       interface display;module procedure display_matrix_free_params;       end interface
       interface display;module procedure display_many_matrix_free_params;  end interface
       interface print;  module procedure print_matrix_free_params;         end interface
       interface print;  module procedure print_many_matrix_free_params;    end interface
       interface export; module procedure export_matrix_free_params;        end interface
       interface export; module procedure export_many_matrix_free_params;   end interface
       interface import; module procedure import_matrix_free_params;        end interface
       interface import; module procedure import_many_matrix_free_params;   end interface
       interface export; module procedure export_wrapper_matrix_free_params;end interface
       interface import; module procedure import_wrapper_matrix_free_params;end interface

       type MATRIX_FREE_PARAMS
         private
         logical :: suppress_warning = .false.
         real(cp) :: alpha = 0.0_cp
         real(cp) :: beta = 0.0_cp
         real(cp) :: coeff_natural = 0.0_cp
         real(cp) :: coeff_explicit = 0.0_cp
         real(cp) :: coeff_implicit = 0.0_cp
         real(cp) :: coeff_implicit_time_split = 0.0_cp
       end type

       contains

       subroutine init_MATRIX_FREE_PARAMS(this,that)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         type(matrix_free_params),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         this%alpha = that%alpha
         this%beta = that%beta
         this%coeff_natural = that%coeff_natural
         this%coeff_explicit = that%coeff_explicit
         this%coeff_implicit = that%coeff_implicit
         this%coeff_implicit_time_split = that%coeff_implicit_time_split
       end subroutine

       subroutine init_many_MATRIX_FREE_PARAMS(this,that)
         implicit none
         type(matrix_free_params),dimension(:),intent(inout) :: this
         type(matrix_free_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MATRIX_FREE_PARAMS(this)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         this%suppress_warning = .false.
         this%alpha = 0.0_cp
         this%beta = 0.0_cp
         this%coeff_natural = 0.0_cp
         this%coeff_explicit = 0.0_cp
         this%coeff_implicit = 0.0_cp
         this%coeff_implicit_time_split = 0.0_cp
       end subroutine

       subroutine delete_many_MATRIX_FREE_PARAMS(this)
         implicit none
         type(matrix_free_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning          = ',this%suppress_warning
         write(un,*) 'alpha                     = ',this%alpha
         write(un,*) 'beta                      = ',this%beta
         write(un,*) 'coeff_natural             = ',this%coeff_natural
         write(un,*) 'coeff_explicit            = ',this%coeff_explicit
         write(un,*) 'coeff_implicit            = ',this%coeff_implicit
         write(un,*) 'coeff_implicit_time_split = ',this%coeff_implicit_time_split
       end subroutine

       subroutine display_many_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MATRIX_FREE_PARAMS(this)
         implicit none
         type(matrix_free_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MATRIX_FREE_PARAMS(this)
         implicit none
         type(matrix_free_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%suppress_warning
         write(un,*) this%alpha
         write(un,*) this%beta
         write(un,*) this%coeff_natural
         write(un,*) this%coeff_explicit
         write(un,*) this%coeff_implicit
         write(un,*) this%coeff_implicit_time_split
       end subroutine

       subroutine export_many_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%suppress_warning
         read(un,*) this%alpha
         read(un,*) this%beta
         read(un,*) this%coeff_natural
         read(un,*) this%coeff_explicit
         read(un,*) this%coeff_implicit
         read(un,*) this%coeff_implicit_time_split
       end subroutine

       subroutine import_many_MATRIX_FREE_PARAMS(this,un)
         implicit none
         type(matrix_free_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MATRIX_FREE_PARAMS(this,dir,name)
         implicit none
         type(matrix_free_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MATRIX_FREE_PARAMS(this,dir,name)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module