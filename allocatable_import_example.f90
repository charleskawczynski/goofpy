       subroutine export_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         integer :: n_base
         if (allocated(this%s_base)) then
           n_base = size(this%s_base)
           write(un,*) n_base
           if (n_base.gt.0) then
             do i_iter=1,n_base
               call export(this%s_base(i_iter),un)
             enddo
           endif
         else
           write(un,*) 0
         endif
       end subroutine

       subroutine import_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         integer :: n_base
         call delete(this)
         read(un,*) n_base
         if (n_base.gt.0) then
           allocate(this%s_base(n_base))
           do i_iter=1,n_base
             call import(this%s_base(i_iter),un)
           enddo
         endif
       end subroutine
