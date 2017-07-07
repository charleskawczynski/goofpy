       module direction_mod
       use allClassFuncs_mod
       use constants_mod
       use vector_mod


       type direction
         private
         type(vector) :: initial
         type(vector) :: instant
         type(vector) :: final
         real(cp) :: theta_i
         real(cp) :: theta
         real(cp) :: theta_f
         real(cp) :: phi_i
       endtype

       contains

       function setDirection(initial,instant,final,theta_i,theta,
     &   theta_f,phi_i) result(this)
         implicit none
         type(direction) :: this
         type(vector), intent(in) :: initial
         type(vector), intent(in) :: instant
         type(vector), intent(in) :: final
         real(cp), intent(in) :: theta_i
         real(cp), intent(in) :: theta
         real(cp), intent(in) :: theta_f
         real(cp), intent(in) :: phi_i
         call SetDirectionInitial(this,initial)
         call SetDirectionInstant(this,instant)
         call SetDirectionFinal(this,final)
         call SetDirectionTheta_i(this,theta_i)
         call SetDirectionTheta(this,theta)
         call SetDirectionTheta_f(this,theta_f)
         call SetDirectionPhi_i(this,phi_i)
       end function

       subroutine setDirectionInitial(this,initial)
         implicit none
         type(direction),intent(inout) :: this
         type(vector), intent(in) :: initial
         this%initial = initial
       end subroutine

       subroutine setDirectionInstant(this,instant)
         implicit none
         type(direction),intent(inout) :: this
         type(vector), intent(in) :: instant
         this%instant = instant
       end subroutine

       subroutine setDirectionFinal(this,final)
         implicit none
         type(direction),intent(inout) :: this
         type(vector), intent(in) :: final
         this%final = final
       end subroutine

       subroutine setDirectionTheta_i(this,theta_i)
         implicit none
         type(direction),intent(inout) :: this
         real(cp), intent(in) :: theta_i
         this%theta_i = theta_i
       end subroutine

       subroutine setDirectionTheta(this,theta)
         implicit none
         type(direction),intent(inout) :: this
         real(cp), intent(in) :: theta
         this%theta = theta
       end subroutine

       subroutine setDirectionTheta_f(this,theta_f)
         implicit none
         type(direction),intent(inout) :: this
         real(cp), intent(in) :: theta_f
         this%theta_f = theta_f
       end subroutine

       subroutine setDirectionPhi_i(this,phi_i)
         implicit none
         type(direction),intent(inout) :: this
         real(cp), intent(in) :: phi_i
         this%phi_i = phi_i
       end subroutine

       function getDirectionInitial(this) result(initial)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: initial
         initial = this%initial
       end function

       function getDirectionInstant(this) result(instant)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: instant
         instant = this%instant
       end function

       function getDirectionFinal(this) result(final)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: final
         final = this%final
       end function

       function getDirectionTheta_i(this) result(theta_i)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_i
         theta_i = this%theta_i
       end function

       function getDirectionTheta(this) result(theta)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta
         theta = this%theta
       end function

       function getDirectionTheta_f(this) result(theta_f)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_f
         theta_f = this%theta_f
       end function

       function getDirectionPhi_i(this) result(phi_i)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: phi_i
         phi_i = this%phi_i
       end function

       subroutine getDirection(this,initial,instant,final,theta_i,
     &   theta,theta_f,phi_i)
         implicit none
         type(direction),intent(in) :: this
         type(vector), intent(out) :: initial
         type(vector), intent(out) :: instant
         type(vector), intent(out) :: final
         real(cp), intent(out) :: theta_i
         real(cp), intent(out) :: theta
         real(cp), intent(out) :: theta_f
         real(cp), intent(out) :: phi_i
         initial = getDirectionInitial(this)
         instant = getDirectionInstant(this)
         final = getDirectionFinal(this)
         theta_i = getDirectionTheta_i(this)
         theta = getDirectionTheta(this)
         theta_f = getDirectionTheta_f(this)
         phi_i = getDirectionPhi_i(this)
       end subroutine

       subroutine printDirectionInitial(this)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: initial
         call printVector(this%initial)
       end subroutine

       subroutine printDirectionInstant(this)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: instant
         call printVector(this%instant)
       end subroutine

       subroutine printDirectionFinal(this)
         implicit none
         type(direction),intent(in) :: this
         type(vector) :: final
         call printVector(this%final)
       end subroutine

       subroutine printDirectionTheta_i(this)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_i
         write(*,*) "theta_i: ", this%theta_i
       end subroutine

       subroutine printDirectionTheta(this)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta
         write(*,*) "theta: ", this%theta
       end subroutine

       subroutine printDirectionTheta_f(this)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_f
         write(*,*) "theta_f: ", this%theta_f
       end subroutine

       subroutine printDirectionPhi_i(this)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: phi_i
         write(*,*) "phi_i: ", this%phi_i
       end subroutine

       subroutine printDirection(this)
         implicit none
         type(direction), intent(in) :: this
         write(*,*) 'Printed data for direction'
         write(*,*) '***************'
         call printVector(this%initial)
         call printVector(this%instant)
         call printVector(this%final)
         call printDirectionTheta_i(this)
         call printDirectionTheta(this)
         call printDirectionTheta_f(this)
         call printDirectionPhi_i(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeDirectionInitial(this,dir)
         implicit none
         type(direction),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: initial
         call writeVector(this%initial,dir)
       end subroutine

       subroutine writeDirectionInstant(this,dir)
         implicit none
         type(direction),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: instant
         call writeVector(this%instant,dir)
       end subroutine

       subroutine writeDirectionFinal(this,dir)
         implicit none
         type(direction),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: final
         call writeVector(this%final,dir)
       end subroutine

       subroutine writeDirectionTheta_i(this,dir,parentUnit)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_i
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "theta_i: ", this%theta_i
         else
           call newAndOpen(dir,"theta_i")
           write(NewU,*) "theta_i: ", this%theta_i
           call closeAndMessage(NewU,"theta_i")
         endif
       end subroutine

       subroutine writeDirectionTheta(this,dir,parentUnit)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "theta: ", this%theta
         else
           call newAndOpen(dir,"theta")
           write(NewU,*) "theta: ", this%theta
           call closeAndMessage(NewU,"theta")
         endif
       end subroutine

       subroutine writeDirectionTheta_f(this,dir,parentUnit)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: theta_f
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "theta_f: ", this%theta_f
         else
           call newAndOpen(dir,"theta_f")
           write(NewU,*) "theta_f: ", this%theta_f
           call closeAndMessage(NewU,"theta_f")
         endif
       end subroutine

       subroutine writeDirectionPhi_i(this,dir,parentUnit)
         implicit none
         type(direction),intent(in) :: this
         real(cp) :: phi_i
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "phi_i: ", this%phi_i
         else
           call newAndOpen(dir,"phi_i")
           write(NewU,*) "phi_i: ", this%phi_i
           call closeAndMessage(NewU,"phi_i")
         endif
       end subroutine

       subroutine writeDirection(this,dir,parentUnit)
         implicit none
         type(direction), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Direction.txt")
         endif
         call writeVector(this%initial,dir,NewU)
         call writeVector(this%instant,dir,NewU)
         call writeVector(this%final,dir,NewU)
         call writeDirectionTheta_i(this,dir,NewU)
         call writeDirectionTheta(this,dir,NewU)
         call writeDirectionTheta_f(this,dir,NewU)
         call writeDirectionPhi_i(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for direction written to file +++'
         endif
       end subroutine

         include '../helper/direction_helper.f'

       end module direction_mod