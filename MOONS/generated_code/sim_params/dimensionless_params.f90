       module DIMENSIONLESS_PARAMS_mod
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
       public :: DIMENSIONLESS_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_dimensionless_params;          end interface
       interface init;   module procedure init_many_dimensionless_params;     end interface
       interface delete; module procedure delete_dimensionless_params;        end interface
       interface delete; module procedure delete_many_dimensionless_params;   end interface
       interface display;module procedure display_dimensionless_params;       end interface
       interface display;module procedure display_many_dimensionless_params;  end interface
       interface print;  module procedure print_dimensionless_params;         end interface
       interface print;  module procedure print_many_dimensionless_params;    end interface
       interface export; module procedure export_dimensionless_params;        end interface
       interface export; module procedure export_many_dimensionless_params;   end interface
       interface import; module procedure import_dimensionless_params;        end interface
       interface import; module procedure import_many_dimensionless_params;   end interface
       interface export; module procedure export_wrapper_dimensionless_params;end interface
       interface import; module procedure import_wrapper_dimensionless_params;end interface

       type DIMENSIONLESS_PARAMS
         private
         real(cp) :: re = 0.0_cp
         real(cp) :: al = 0.0_cp
         real(cp) :: n = 0.0_cp
         real(cp) :: ha = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: gr = 0.0_cp
         real(cp) :: fr = 0.0_cp
         real(cp) :: pr = 0.0_cp
         real(cp) :: pe = 0.0_cp
         real(cp) :: ec = 0.0_cp
         real(cp) :: rem = 0.0_cp
         real(cp),dimension(6) :: c_w = 0.0_cp
         real(cp),dimension(6) :: robin_coeff = 0.0_cp
         real(cp) :: q = 0.0_cp
         real(cp) :: sig_local_over_sig_f = 0.0_cp
         real(cp) :: ke_scale = 0.0_cp
         real(cp) :: me_scale = 0.0_cp
         real(cp) :: je_scale = 0.0_cp
         real(cp) :: l_eta = 0.0_cp
         real(cp) :: u_eta = 0.0_cp
         real(cp) :: t_eta = 0.0_cp
       end type

       contains

       subroutine init_DIMENSIONLESS_PARAMS(this,that)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         type(dimensionless_params),intent(in) :: that
         call delete(this)
         this%re = that%re
         this%al = that%al
         this%n = that%n
         this%ha = that%ha
         this%tau = that%tau
         this%gr = that%gr
         this%fr = that%fr
         this%pr = that%pr
         this%pe = that%pe
         this%ec = that%ec
         this%rem = that%rem
         this%c_w = that%c_w
         this%robin_coeff = that%robin_coeff
         this%q = that%q
         this%sig_local_over_sig_f = that%sig_local_over_sig_f
         this%ke_scale = that%ke_scale
         this%me_scale = that%me_scale
         this%je_scale = that%je_scale
         this%l_eta = that%l_eta
         this%u_eta = that%u_eta
         this%t_eta = that%t_eta
       end subroutine

       subroutine init_many_DIMENSIONLESS_PARAMS(this,that)
         implicit none
         type(dimensionless_params),dimension(:),intent(inout) :: this
         type(dimensionless_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_DIMENSIONLESS_PARAMS(this)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         this%re = 0.0_cp
         this%al = 0.0_cp
         this%n = 0.0_cp
         this%ha = 0.0_cp
         this%tau = 0.0_cp
         this%gr = 0.0_cp
         this%fr = 0.0_cp
         this%pr = 0.0_cp
         this%pe = 0.0_cp
         this%ec = 0.0_cp
         this%rem = 0.0_cp
         this%c_w = 0.0_cp
         this%robin_coeff = 0.0_cp
         this%q = 0.0_cp
         this%sig_local_over_sig_f = 0.0_cp
         this%ke_scale = 0.0_cp
         this%me_scale = 0.0_cp
         this%je_scale = 0.0_cp
         this%l_eta = 0.0_cp
         this%u_eta = 0.0_cp
         this%t_eta = 0.0_cp
       end subroutine

       subroutine delete_many_DIMENSIONLESS_PARAMS(this)
         implicit none
         type(dimensionless_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 're                   = ',this%re
         write(un,*) 'al                   = ',this%al
         write(un,*) 'n                    = ',this%n
         write(un,*) 'ha                   = ',this%ha
         write(un,*) 'tau                  = ',this%tau
         write(un,*) 'gr                   = ',this%gr
         write(un,*) 'fr                   = ',this%fr
         write(un,*) 'pr                   = ',this%pr
         write(un,*) 'pe                   = ',this%pe
         write(un,*) 'ec                   = ',this%ec
         write(un,*) 'rem                  = ',this%rem
         write(un,*) 'c_w                  = ',this%c_w
         write(un,*) 'robin_coeff          = ',this%robin_coeff
         write(un,*) 'q                    = ',this%q
         write(un,*) 'sig_local_over_sig_f = ',this%sig_local_over_sig_f
         write(un,*) 'ke_scale             = ',this%ke_scale
         write(un,*) 'me_scale             = ',this%me_scale
         write(un,*) 'je_scale             = ',this%je_scale
         write(un,*) 'l_eta                = ',this%l_eta
         write(un,*) 'u_eta                = ',this%u_eta
         write(un,*) 't_eta                = ',this%t_eta
       end subroutine

       subroutine display_many_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_DIMENSIONLESS_PARAMS(this)
         implicit none
         type(dimensionless_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_DIMENSIONLESS_PARAMS(this)
         implicit none
         type(dimensionless_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%re
         write(un,*) this%al
         write(un,*) this%n
         write(un,*) this%ha
         write(un,*) this%tau
         write(un,*) this%gr
         write(un,*) this%fr
         write(un,*) this%pr
         write(un,*) this%pe
         write(un,*) this%ec
         write(un,*) this%rem
         write(un,*) this%c_w
         write(un,*) this%robin_coeff
         write(un,*) this%q
         write(un,*) this%sig_local_over_sig_f
         write(un,*) this%ke_scale
         write(un,*) this%me_scale
         write(un,*) this%je_scale
         write(un,*) this%l_eta
         write(un,*) this%u_eta
         write(un,*) this%t_eta
       end subroutine

       subroutine export_many_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%re
         read(un,*) this%al
         read(un,*) this%n
         read(un,*) this%ha
         read(un,*) this%tau
         read(un,*) this%gr
         read(un,*) this%fr
         read(un,*) this%pr
         read(un,*) this%pe
         read(un,*) this%ec
         read(un,*) this%rem
         read(un,*) this%c_w
         read(un,*) this%robin_coeff
         read(un,*) this%q
         read(un,*) this%sig_local_over_sig_f
         read(un,*) this%ke_scale
         read(un,*) this%me_scale
         read(un,*) this%je_scale
         read(un,*) this%l_eta
         read(un,*) this%u_eta
         read(un,*) this%t_eta
       end subroutine

       subroutine import_many_DIMENSIONLESS_PARAMS(this,un)
         implicit none
         type(dimensionless_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_DIMENSIONLESS_PARAMS(this,dir,name)
         implicit none
         type(dimensionless_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_DIMENSIONLESS_PARAMS(this,dir,name)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module