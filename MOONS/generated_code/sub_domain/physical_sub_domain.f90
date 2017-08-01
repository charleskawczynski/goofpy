       module PHYSICAL_SUB_DOMAIN_mod
       use IO_tools_mod
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
       public :: PHYSICAL_SUB_DOMAIN
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_physical_sub_domain;          end interface
       interface init;   module procedure init_many_physical_sub_domain;     end interface
       interface delete; module procedure delete_physical_sub_domain;        end interface
       interface delete; module procedure delete_many_physical_sub_domain;   end interface
       interface display;module procedure display_physical_sub_domain;       end interface
       interface display;module procedure display_many_physical_sub_domain;  end interface
       interface print;  module procedure print_physical_sub_domain;         end interface
       interface print;  module procedure print_many_physical_sub_domain;    end interface
       interface export; module procedure export_physical_sub_domain;        end interface
       interface export; module procedure export_many_physical_sub_domain;   end interface
       interface import; module procedure import_physical_sub_domain;        end interface
       interface import; module procedure import_many_physical_sub_domain;   end interface
       interface export; module procedure export_wrapper_physical_sub_domain;end interface
       interface import; module procedure import_wrapper_physical_sub_domain;end interface

       type PHYSICAL_SUB_DOMAIN
         private
         type(sub_domain) :: total
         type(sub_domain) :: physical
         logical :: defined = .false.
       end type

       contains

       subroutine init_PHYSICAL_SUB_DOMAIN(this,that)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         type(physical_sub_domain),intent(in) :: that
         call delete(this)
         call init(this%total,that%total)
         call init(this%physical,that%physical)
         this%defined = that%defined
       end subroutine

       subroutine init_many_PHYSICAL_SUB_DOMAIN(this,that)
         implicit none
         type(physical_sub_domain),dimension(:),intent(inout) :: this
         type(physical_sub_domain),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_PHYSICAL_SUB_DOMAIN(this)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         call delete(this%total)
         call delete(this%physical)
         this%defined = .false.
       end subroutine

       subroutine delete_many_PHYSICAL_SUB_DOMAIN(this)
         implicit none
         type(physical_sub_domain),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%total,un)
         call display(this%physical,un)
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine display_many_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_PHYSICAL_SUB_DOMAIN(this)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_PHYSICAL_SUB_DOMAIN(this)
         implicit none
         type(physical_sub_domain),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%total,un)
         call export(this%physical,un)
         write(un,*) this%defined
       end subroutine

       subroutine export_many_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%total,un)
         call import(this%physical,un)
         read(un,*) this%defined
       end subroutine

       subroutine import_many_PHYSICAL_SUB_DOMAIN(this,un)
         implicit none
         type(physical_sub_domain),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_PHYSICAL_SUB_DOMAIN(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_PHYSICAL_SUB_DOMAIN(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module