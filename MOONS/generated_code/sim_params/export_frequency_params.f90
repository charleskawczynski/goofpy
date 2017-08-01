       module EXPORT_FREQUENCY_PARAMS_mod
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
       public :: EXPORT_FREQUENCY_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_frequency_params;          end interface
       interface init;   module procedure init_many_export_frequency_params;     end interface
       interface delete; module procedure delete_export_frequency_params;        end interface
       interface delete; module procedure delete_many_export_frequency_params;   end interface
       interface display;module procedure display_export_frequency_params;       end interface
       interface display;module procedure display_many_export_frequency_params;  end interface
       interface print;  module procedure print_export_frequency_params;         end interface
       interface print;  module procedure print_many_export_frequency_params;    end interface
       interface export; module procedure export_export_frequency_params;        end interface
       interface export; module procedure export_many_export_frequency_params;   end interface
       interface import; module procedure import_export_frequency_params;        end interface
       interface import; module procedure import_many_export_frequency_params;   end interface
       interface export; module procedure export_wrapper_export_frequency_params;end interface
       interface import; module procedure import_wrapper_export_frequency_params;end interface

       type EXPORT_FREQUENCY_PARAMS
         private
         logical :: export_ever = .false.
         logical :: export_first_step = .false.
         logical :: export_now = .false.
         integer :: frequency_coeff = 0
         integer :: frequency_base = 0
         integer :: frequency_exp = 0
       end type

       contains

       subroutine init_EXPORT_FREQUENCY_PARAMS(this,that)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         type(export_frequency_params),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%export_first_step = that%export_first_step
         this%export_now = that%export_now
         this%frequency_coeff = that%frequency_coeff
         this%frequency_base = that%frequency_base
         this%frequency_exp = that%frequency_exp
       end subroutine

       subroutine init_many_EXPORT_FREQUENCY_PARAMS(this,that)
         implicit none
         type(export_frequency_params),dimension(:),intent(inout) :: this
         type(export_frequency_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EXPORT_FREQUENCY_PARAMS(this)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         this%export_ever = .false.
         this%export_first_step = .false.
         this%export_now = .false.
         this%frequency_coeff = 0
         this%frequency_base = 0
         this%frequency_exp = 0
       end subroutine

       subroutine delete_many_EXPORT_FREQUENCY_PARAMS(this)
         implicit none
         type(export_frequency_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever       = ',this%export_ever
         write(un,*) 'export_first_step = ',this%export_first_step
         write(un,*) 'export_now        = ',this%export_now
         write(un,*) 'frequency_coeff   = ',this%frequency_coeff
         write(un,*) 'frequency_base    = ',this%frequency_base
         write(un,*) 'frequency_exp     = ',this%frequency_exp
       end subroutine

       subroutine display_many_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EXPORT_FREQUENCY_PARAMS(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EXPORT_FREQUENCY_PARAMS(this)
         implicit none
         type(export_frequency_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%export_ever
         write(un,*) this%export_first_step
         write(un,*) this%export_now
         write(un,*) this%frequency_coeff
         write(un,*) this%frequency_base
         write(un,*) this%frequency_exp
       end subroutine

       subroutine export_many_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%export_ever
         read(un,*) this%export_first_step
         read(un,*) this%export_now
         read(un,*) this%frequency_coeff
         read(un,*) this%frequency_base
         read(un,*) this%frequency_exp
       end subroutine

       subroutine import_many_EXPORT_FREQUENCY_PARAMS(this,un)
         implicit none
         type(export_frequency_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EXPORT_FREQUENCY_PARAMS(this,dir,name)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EXPORT_FREQUENCY_PARAMS(this,dir,name)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module