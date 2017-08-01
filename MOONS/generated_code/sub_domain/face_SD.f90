       module FACE_SD_mod
       use IO_tools_mod
       use index_2D_mod
       use sub_domain_mod
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
       public :: FACE_SD
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_face_sd;          end interface
       interface init;   module procedure init_many_face_sd;     end interface
       interface delete; module procedure delete_face_sd;        end interface
       interface delete; module procedure delete_many_face_sd;   end interface
       interface display;module procedure display_face_sd;       end interface
       interface display;module procedure display_many_face_sd;  end interface
       interface print;  module procedure print_face_sd;         end interface
       interface print;  module procedure print_many_face_sd;    end interface
       interface export; module procedure export_face_sd;        end interface
       interface export; module procedure export_many_face_sd;   end interface
       interface import; module procedure import_face_sd;        end interface
       interface import; module procedure import_many_face_sd;   end interface
       interface export; module procedure export_wrapper_face_sd;end interface
       interface import; module procedure import_wrapper_face_sd;end interface

       type FACE_SD
         private
         integer :: s = 0
         type(sub_domain),dimension(6) :: g
         type(sub_domain),dimension(6) :: g_periodic_n
         type(sub_domain),dimension(6) :: b
         type(sub_domain),dimension(6) :: i
         type(sub_domain),dimension(6) :: i_opp
         type(sub_domain),dimension(6) :: i_opp_periodic_n
         type(index_2d),dimension(6) :: i_2d
         real(cp),dimension(6) :: dh = 0.0_cp
         real(cp),dimension(6) :: nhat = 0.0_cp
         real(cp),dimension(6) :: c_w = 0.0_cp
         real(cp),dimension(6) :: robin_coeff = 0.0_cp
       end type

       contains

       subroutine init_FACE_SD(this,that)
         implicit none
         type(face_sd),intent(inout) :: this
         type(face_sd),intent(in) :: that
         call delete(this)
         this%s = that%s
         call init(this%g,that%g)
         call init(this%g_periodic_n,that%g_periodic_n)
         call init(this%b,that%b)
         call init(this%i,that%i)
         call init(this%i_opp,that%i_opp)
         call init(this%i_opp_periodic_n,that%i_opp_periodic_n)
         call init(this%i_2d,that%i_2d)
         this%dh = that%dh
         this%nhat = that%nhat
         this%c_w = that%c_w
         this%robin_coeff = that%robin_coeff
       end subroutine

       subroutine init_many_FACE_SD(this,that)
         implicit none
         type(face_sd),dimension(:),intent(inout) :: this
         type(face_sd),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_FACE_SD(this)
         implicit none
         type(face_sd),intent(inout) :: this
         this%s = 0
         call delete(this%g)
         call delete(this%g_periodic_n)
         call delete(this%b)
         call delete(this%i)
         call delete(this%i_opp)
         call delete(this%i_opp_periodic_n)
         call delete(this%i_2d)
         this%dh = 0.0_cp
         this%nhat = 0.0_cp
         this%c_w = 0.0_cp
         this%robin_coeff = 0.0_cp
       end subroutine

       subroutine delete_many_FACE_SD(this)
         implicit none
         type(face_sd),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_FACE_SD(this,un)
         implicit none
         type(face_sd),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 's                = ',this%s
         call display(this%g,un)
         call display(this%g_periodic_n,un)
         call display(this%b,un)
         call display(this%i,un)
         call display(this%i_opp,un)
         call display(this%i_opp_periodic_n,un)
         call display(this%i_2d,un)
         write(un,*) 'dh               = ',this%dh
         write(un,*) 'nhat             = ',this%nhat
         write(un,*) 'c_w              = ',this%c_w
         write(un,*) 'robin_coeff      = ',this%robin_coeff
       end subroutine

       subroutine display_many_FACE_SD(this,un)
         implicit none
         type(face_sd),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_FACE_SD(this)
         implicit none
         type(face_sd),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_FACE_SD(this)
         implicit none
         type(face_sd),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_FACE_SD(this,un)
         implicit none
         type(face_sd),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%s
         call export(this%g,un)
         call export(this%g_periodic_n,un)
         call export(this%b,un)
         call export(this%i,un)
         call export(this%i_opp,un)
         call export(this%i_opp_periodic_n,un)
         call export(this%i_2d,un)
         write(un,*) this%dh
         write(un,*) this%nhat
         write(un,*) this%c_w
         write(un,*) this%robin_coeff
       end subroutine

       subroutine export_many_FACE_SD(this,un)
         implicit none
         type(face_sd),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_FACE_SD(this,un)
         implicit none
         type(face_sd),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%s
         call import(this%g,un)
         call import(this%g_periodic_n,un)
         call import(this%b,un)
         call import(this%i,un)
         call import(this%i_opp,un)
         call import(this%i_opp_periodic_n,un)
         call import(this%i_2d,un)
         read(un,*) this%dh
         read(un,*) this%nhat
         read(un,*) this%c_w
         read(un,*) this%robin_coeff
       end subroutine

       subroutine import_many_FACE_SD(this,un)
         implicit none
         type(face_sd),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_FACE_SD(this,dir,name)
         implicit none
         type(face_sd),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_FACE_SD(this,dir,name)
         implicit none
         type(face_sd),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module