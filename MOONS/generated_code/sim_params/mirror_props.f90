       module MIRROR_PROPS_mod
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
       public :: MIRROR_PROPS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mirror_props;          end interface
       interface init;   module procedure init_many_mirror_props;     end interface
       interface delete; module procedure delete_mirror_props;        end interface
       interface delete; module procedure delete_many_mirror_props;   end interface
       interface display;module procedure display_mirror_props;       end interface
       interface display;module procedure display_many_mirror_props;  end interface
       interface print;  module procedure print_mirror_props;         end interface
       interface print;  module procedure print_many_mirror_props;    end interface
       interface export; module procedure export_mirror_props;        end interface
       interface export; module procedure export_many_mirror_props;   end interface
       interface import; module procedure import_mirror_props;        end interface
       interface import; module procedure import_many_mirror_props;   end interface
       interface export; module procedure export_wrapper_mirror_props;end interface
       interface import; module procedure import_wrapper_mirror_props;end interface

       type MIRROR_PROPS
         private
         logical :: mirror = .false.
         integer :: mirror_face = 0
         real(cp),dimension(3) :: mirror_sign = 0.0_cp
         real(cp),dimension(3) :: mirror_sign_a = 0.0_cp
       end type

       contains

       subroutine init_MIRROR_PROPS(this,that)
         implicit none
         type(mirror_props),intent(inout) :: this
         type(mirror_props),intent(in) :: that
         call delete(this)
         this%mirror = that%mirror
         this%mirror_face = that%mirror_face
         this%mirror_sign = that%mirror_sign
         this%mirror_sign_a = that%mirror_sign_a
       end subroutine

       subroutine init_many_MIRROR_PROPS(this,that)
         implicit none
         type(mirror_props),dimension(:),intent(inout) :: this
         type(mirror_props),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MIRROR_PROPS(this)
         implicit none
         type(mirror_props),intent(inout) :: this
         this%mirror = .false.
         this%mirror_face = 0
         this%mirror_sign = 0.0_cp
         this%mirror_sign_a = 0.0_cp
       end subroutine

       subroutine delete_many_MIRROR_PROPS(this)
         implicit none
         type(mirror_props),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'mirror        = ',this%mirror
         write(un,*) 'mirror_face   = ',this%mirror_face
         write(un,*) 'mirror_sign   = ',this%mirror_sign
         write(un,*) 'mirror_sign_a = ',this%mirror_sign_a
       end subroutine

       subroutine display_many_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MIRROR_PROPS(this)
         implicit none
         type(mirror_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MIRROR_PROPS(this)
         implicit none
         type(mirror_props),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%mirror
         write(un,*) this%mirror_face
         write(un,*) this%mirror_sign
         write(un,*) this%mirror_sign_a
       end subroutine

       subroutine export_many_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%mirror
         read(un,*) this%mirror_face
         read(un,*) this%mirror_sign
         read(un,*) this%mirror_sign_a
       end subroutine

       subroutine import_many_MIRROR_PROPS(this,un)
         implicit none
         type(mirror_props),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MIRROR_PROPS(this,dir,name)
         implicit none
         type(mirror_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MIRROR_PROPS(this,dir,name)
         implicit none
         type(mirror_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module