program poscar2data
        use JG
        use atom_information
        implicit none
!------------------------------------------------------------------------------------------
        real :: a(3),b(3),c(3)
        integer,allocatable :: n_atom(:)
        character :: file_name
        character(len=128) :: file1,file2
        real :: factor
        real,allocatable :: a_mass(:)
        integer :: i,j,k,counts
        integer :: n,atom_number,m=1
        character(len=20) :: sit
        type :: atom
                integer :: element
                real :: x(3)
        end type atom
        type(atom),allocatable :: R(:)
        type(str_array),allocatable :: atom_type(:)
        character(len=1024) :: temp_s
!-----------------------------------------------------------------------------------------
        call get_command_argument(1,file1)
        call get_command_argument(2,file2)
        if (len_trim(adjustl(file1)) == 0) then
                file1='POSCAR'
        end if

        if (len_trim(adjustl(file2)) == 0) then
                file2='lammps.data'
        end if

        open(unit = 100,file = trim(adjustl(file1)),status = 'old',action = 'read')
        open(unit = 200,file = trim(adjustl(file2)),status = 'replace')
!--------------------------------read file------------------------------------------------
        read(100,*)
        read(100,"(F5.2)") factor
        read(100,*) a(1),a(2),a(3)
        read(100,*) b(1),b(2),b(3)
        read(100,*) c(1),c(2),c(3)
        read(100,"(A)") temp_s
        call get_colume(temp_s,atom_type)
        atom_number = size(atom_type)

        allocate(n_atom(atom_number))
        allocate(a_mass(atom_number))

        do i=1,atom_number
                call mass_information(atom_type(i)%str,a_mass(i))
        end do
        n_atom(:) = 0
        read(100,*) (n_atom(i),i=1,atom_number)
        read(100,*) sit
        i = 0
        j = 0
        k = 0
        n = 0
        counts=1

        do i=1,atom_number
                n=n+n_atom(i)
        end do

        allocate(R(n))
        m=1 
        do i=1,atom_number
                do j=1,n_atom(i)
                        read(100,*) R(m)%x(1),R(m)%x(2),R(m)%x(3)
                        R(m)%element = i
                        m=m+1
                end do
        end do

                i = 0
                j = 0
                k = 0

!--------------------------------------------------------------write file-----------------------------------------------------
        write(200,"(A31)") "# Lammps file written by Z.F.Wu"
        write(200,*)
        write(200,"(I0,2X,5A)") n,"atoms"
        write(200,"(I0,2X,10A)") atom_number,"atom types"
        write(200,*)
        write(200,"(F3.1,1X,F9.4,1X,A3,1X,A3)") 0.0,a(1)*factor,"xlo","xhi"
        write(200,"(F3.1,1X,F9.4,1X,A3,1X,A3)") 0.0,b(2)*factor,"ylo","yhi"
        write(200,"(F3.1,1X,F9.4,1X,A3,1X,A3)") 0.0,c(3)*factor,"zlo","zhi"
        
        write(200,*)
        write(200,"(A6)") "Masses"
        write(200,*)
        do i=1,atom_number
                write(200,"(I0,1X,F8.3,2X,A2,A4)") i,a_mass(i),"# ",atom_type(i)%str
        end do
        write(200,*)
        write(200,*) "Atoms  # atomic"
        write(200,*)
        m=1
        if (trim(adjustl(sit)) == 'Cartesian') then
                do i=1,atom_number
                        do j=1,n_atom(i)
                                write(200,"(I0,1X,I0,1X,3(F15.8,1X))") m,R(m)%element,R(m)%x(1)*factor,R(m)%x(2)*factor,&
                                R(m)%x(3)*factor
                                m=m+1
                        end do
                end do
                write(*,*) m
        else if (trim(adjustl(sit)) == 'Direct') then
                do i=1,atom_number
                        do j=1,n_atom(i)
                                do k=1,3
                                        R(m)%x(k)=(a(k)*R(m)%x(1)+b(k)*R(m)%x(2)+c(k)*R(m)%x(3))*factor
                                end do

                                write(200,"(I0,1X,I0,1X,3(F15.8,1X))") m,R(m)%element,R(m)%x(1),R(m)%x(2),R(m)%x(3)
                                m=m+1
                                end do
                end do
                write(*,*) m
         end if 
!------------------------------------------------------------------------------------------------------------
stop
end program






                

