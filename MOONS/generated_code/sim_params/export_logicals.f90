       module EXPORT_LOGICALS_mod
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
       public :: EXPORT_LOGICALS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_logicals;          end interface
       interface init;   module procedure init_many_export_logicals;     end interface
       interface delete; module procedure delete_export_logicals;        end interface
       interface delete; module procedure delete_many_export_logicals;   end interface
       interface display;module procedure display_export_logicals;       end interface
       interface display;module procedure display_many_export_logicals;  end interface
       interface print;  module procedure print_export_logicals;         end interface
       interface print;  module procedure print_many_export_logicals;    end interface
       interface export; module procedure export_export_logicals;        end interface
       interface export; module procedure export_many_export_logicals;   end interface
       interface import; module procedure import_export_logicals;        end interface
       interface import; module procedure import_many_export_logicals;   end interface
       interface export; module procedure export_wrapper_export_logicals;end interface
       interface import; module procedure import_wrapper_export_logicals;end interface

       type EXPORT_LOGICALS
         private
         logical :: export_analytic = .false.
         logical :: export_meshes = .false.
         logical :: export_vort_sf = .false.
         logical :: export_mat_props = .false.
         logical :: export_cell_volume = .false.
         logical :: export_ics = .false.
         logical :: export_planar = .false.
         logical :: export_symmetric = .false.
         logical :: export_mesh_block = .false.
         logical :: export_soln_only = .false.
         logical :: defined = .false.
       end type

       contains

       subroutine init_EXPORT_LOGICALS(this,that)
         implicit none
         type(export_logicals),intent(inout) :: this
         type(export_logicals),intent(in) :: that
         call delete(this)
         this%export_analytic = that%export_analytic
         this%export_meshes = that%export_meshes
         this%export_vort_sf = that%export_vort_sf
         this%export_mat_props = that%export_mat_props
         this%export_cell_volume = that%export_cell_volume
         this%export_ics = that%export_ics
         this%export_planar = that%export_planar
         this%export_symmetric = that%export_symmetric
         this%export_mesh_block = that%export_mesh_block
         this%export_soln_only = that%export_soln_only
         this%defined = that%defined
       end subroutine

       subroutine init_many_EXPORT_LOGICALS(this,that)
         implicit none
         type(export_logicals),dimension(:),intent(inout) :: this
         type(export_logicals),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EXPORT_LOGICALS(this)
         implicit none
         type(export_logicals),intent(inout) :: this
         this%export_analytic = .false.
         this%export_meshes = .false.
         this%export_vort_sf = .false.
         this%export_mat_props = .false.
         this%export_cell_volume = .false.
         this%export_ics = .false.
         this%export_planar = .false.
         this%export_symmetric = .false.
         this%export_mesh_block = .false.
         this%export_soln_only = .false.
         this%defined = .false.
       end subroutine

       subroutine delete_many_EXPORT_LOGICALS(this)
         implicit none
         type(export_logicals),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_analytic    = ',this%export_analytic
         write(un,*) 'export_meshes      = ',this%export_meshes
         write(un,*) 'export_vort_sf     = ',this%export_vort_sf
         write(un,*) 'export_mat_props   = ',this%export_mat_props
         write(un,*) 'export_cell_volume = ',this%export_cell_volume
         write(un,*) 'export_ics         = ',this%export_ics
         write(un,*) 'export_planar      = ',this%export_planar
         write(un,*) 'export_symmetric   = ',this%export_symmetric
         write(un,*) 'export_mesh_block  = ',this%export_mesh_block
         write(un,*) 'export_soln_only   = ',this%export_soln_only
         write(un,*) 'defined            = ',this%defined
       end subroutine

       subroutine display_many_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EXPORT_LOGICALS(this)
         implicit none
         type(export_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EXPORT_LOGICALS(this)
         implicit none
         type(export_logicals),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%export_analytic
         write(un,*) this%export_meshes
         write(un,*) this%export_vort_sf
         write(un,*) this%export_mat_props
         write(un,*) this%export_cell_volume
         write(un,*) this%export_ics
         write(un,*) this%export_planar
         write(un,*) this%export_symmetric
         write(un,*) this%export_mesh_block
         write(un,*) this%export_soln_only
         write(un,*) this%defined
       end subroutine

       subroutine export_many_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%export_analytic
         read(un,*) this%export_meshes
         read(un,*) this%export_vort_sf
         read(un,*) this%export_mat_props
         read(un,*) this%export_cell_volume
         read(un,*) this%export_ics
         read(un,*) this%export_planar
         read(un,*) this%export_symmetric
         read(un,*) this%export_mesh_block
         read(un,*) this%export_soln_only
         read(un,*) this%defined
       end subroutine

       subroutine import_many_EXPORT_LOGICALS(this,un)
         implicit none
         type(export_logicals),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EXPORT_LOGICALS(this,dir,name)
         implicit none
         type(export_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EXPORT_LOGICALS(this,dir,name)
         implicit none
         type(export_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module