module charFunctions

    implicit none

contains

!    !-----------------------------------------------------------------------------------------------
!    !-----------------------------------------------------------------------------------------------
!    function string_vec_join(stringVec) result(stringTot)
!
!        implicit none
!
!        !INPUT
!        character (len=*), dimension(:), intent(in) :: stringVec;
!
!        !OUTPUT
!        character (len=200) :: stringTot;
!
!        !LOCAL
!        integer :: i
!
!        !write(*,*) "WRITE Flag string_join"
!        stringTot = ""
!
!        do i = 1, size(stringVec)
!            stringTot = string_join(stringTot, stringVec(i))
!        end do
!
!        stringTot = adjustL(stringTot)
!
!    end function string_vec_join

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function string_join_many(string1, string2, string3, string4, string5&
                              , string6, string7, string8, string9&
                              , string10, string11, string12, string13&
                              , string14, string15, verbose) result(stringTot)

        implicit none

        !INPUT
        character (len=*), intent(in) :: string1;
        character (len=*), intent(in), optional  :: string2;
        character (len=*), intent(in), optional  :: string3;
        character (len=*), intent(in), optional  :: string4;
        character (len=*), intent(in), optional  :: string5;
        character (len=*), intent(in), optional  :: string6;
        character (len=*), intent(in), optional  :: string7;
        character (len=*), intent(in), optional  :: string8;
        character (len=*), intent(in), optional  :: string9;
        character (len=*), intent(in), optional  :: string10;
        character (len=*), intent(in), optional  :: string11;
        character (len=*), intent(in), optional  :: string12;
        character (len=*), intent(in), optional  :: string13;
        character (len=*), intent(in), optional  :: string14;
        character (len=*), intent(in), optional  :: string15;
        logical, intent(in), optional  :: verbose;

        !OUTPUT
        character (len=200) :: stringTot;

        !LOCAL
        integer :: i
        logical :: effecVerb

        effecVerb = .false.
        if(present(verbose)) then
            if(verbose) effecVerb = .true.
        end if

        !write(*,*) "WRITE Flag string_join"
        stringTot = ""
        stringTot = trim(adjustL(string1))
        stringTot = adjustL(stringTot)
        !write(*,*) "string1 = ", string1
        !write(*,*) "string2 = ", string2
        !write(*,*) "string3 = ", string3

        if(effecVerb) write(*,*) "string_join_many INSIDE 1 = "

        do i =1, 200
            if(effecVerb) write(*,*) i, "=", stringTot(i:i)
        end do

        if(effecVerb) write(*,*) "len(trim(stringTot)) = ", len(trim(stringTot))

        if(present(string2)) stringTot = string_join(stringTot, string2, effecVerb)
        if(effecVerb) write(*,*) "string_join_many INSIDE 2 = "

        do i =1, 200
            if(effecVerb) write(*,*) i, "=", stringTot(i:i)
        end do
        if(present(string3)) stringTot = string_join(stringTot, string3, effecVerb)

        if(effecVerb) write(*,*) "string_join_many INSIDE 3 = "

        do i =1, 200
            if(effecVerb) write(*,*) i, "=", stringTot(i:i)
        end do

        if(present(string4)) stringTot = string_join(stringTot, string4, effecVerb)
        if(present(string5)) stringTot = string_join(stringTot, string5, effecVerb)
        if(present(string6)) stringTot = string_join(stringTot, string6, effecVerb)
        if(present(string7)) stringTot = string_join(stringTot, string7, effecVerb)
        if(present(string8)) stringTot = string_join(stringTot, string8, effecVerb)
        if(present(string9)) stringTot = string_join(stringTot, string9, effecVerb)
        if(present(string10)) stringTot = string_join(stringTot, string10, effecVerb)
        if(present(string11)) stringTot = string_join(stringTot, string11, effecVerb)
        if(present(string12)) stringTot = string_join(stringTot, string12, effecVerb)
        if(present(string13)) stringTot = string_join(stringTot, string13, effecVerb)
        if(present(string14)) stringTot = string_join(stringTot, string14, effecVerb)
        if(present(string15)) stringTot = string_join(stringTot, string15, effecVerb)

        stringTot = adjustL(stringTot)

        if(effecVerb) write(*,*) " stringTot = ", stringTot

    end function string_join_many

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function string_join(string1, string2, verbose) result(stringTot)

        implicit none

        !INPUT
        character (len=*)  , intent(in) :: string1, string2;
        logical, intent(in), optional  :: verbose;

        !OUTPUT
        character (len=200) :: stringTot;

        !LOCAL
        logical :: effecVerb

        effecVerb = .false.
        if(present(verbose)) then
            if(verbose) effecVerb = .true.
        end if

        !write(*,*) "WRITE Flag string_join"

        stringTot = trim(adjustL(string1))//trim(adjustL(string2))
        stringTot = adjustL(stringTot)

        if(effecVerb) write(*,*) "Inside string_join: stringTot = ", stringTot

    end function string_join

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function stringNumb_join(string, number) result(stringTot)

        implicit none

        !INPUT
        character (len=*)  , intent(in) :: string
        integer            , intent(in) :: number

        !OUTPUT
        character (len=100) :: stringTot;

        !LOCAL
        character (len=30)  :: nmbString

        !write(*,*) "WRITE Flag stringNumb_join"

        write(nmbString, fmt='(I8)') number
        stringTot = string_join(string, nmbString)

        !write(*,*) "WRITE Flag 2 stringNumb_join"

    end function

    !-----------------------------------------------------------------------------------------------
    !-----------------------------------------------------------------------------------------------
    function numb2String(number, nCharacters) result(stringTot)

        implicit none

        !INPUT
        integer, intent(in) :: number
        integer, intent(in), optional :: nCharacters

        !OUTPUT
        character (len=30) :: stringTot;

        !LOCAL
        character (len=30)  :: nmbString
        integer :: n, i

        write(nmbString, fmt='(I30)') number
        nmbString = adjustL(nmbString)

        if(present(nCharacters)) then
            !write(*,*) "nmbString = ", nmbString
            n = len(trim(nmbString))
            if(n > nCharacters) stop("Inside numb2String the number of characters is to little to represent tis number")

            do i = 1, len(stringTot)
                if(i<=nCharacters) then
                    stringTot(i:i) = "0"
                else
                    stringTot(i:i) = " "
                end if

                !write(*,*) "stringTot = ", stringTot
            end do

            stringTot(nCharacters-n+1:nCharacters) = trim(nmbString)
            stringTot = adjustL(stringTot)

            !write(*,*) "nmbString = ", nmbString
            !write(*,*) "stringTot = ", stringTot
        else
            stringTot = nmbString
        end if

    end function

end module charFunctions
