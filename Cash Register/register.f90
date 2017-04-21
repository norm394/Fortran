! -----------------------------------------------------------------------------
! Author:  Norman Cunningham 
! Date:    04/09/2017
! File:    register.f90
!
! Class:   CIS 343
! Task:    Term Paper program (simple cash register)
! -----------------------------------------------------------------------------

! << Start program. >>
program cash_register
    
    ! << Global Variables. >>
    IMPLICIT NONE
    REAL 			  :: tax_rate     = 1.06
    INTEGER			  :: catch_return = 0
    INTEGER			  :: print_formated     ! Function
	DOUBLE PRECISION  :: get_items  		! Function
	DOUBLE PRECISION  :: get_change  		! Function
	DOUBLE PRECISION  :: end_total     = 0

	! << Prompt user with initial message. >>
	PRINT *, "When finshed enter keyword 'end'"
	PRINT *, ''

    ! << Calculate the total amount due. Calls get_items function. >> 
    end_total = get_items() * tax_rate

    ! << Display the total due, attempt correct formating. >>
    WRITE(*, '(A)', ADVANCE = "NO") "Your total is: $"

    ! << Call the formating function, use new item as a temp. >>
	catch_return = print_formated(end_total)

	PRINT *, ''

    ! << Promt the user for payment. >>
    WRITE(*, '(A)', ADVANCE = "NO") "Enter payment amount: $"
    
    ! << Call get_change function. Prints are handled within function. >>
	catch_return = get_change(end_total)

  	PRINT *, ''

! << End program. >>
end program cash_register



! -----------------------------------------------------------------------------
! << Start function: print_formated >>
!
! This function is used to attempt properly formating a print statement based
! on the size of the value we want to print.
!
! @params (DOUBLE PRECISION input) -The value we are going to print.
! @return -Void return, default by setting to zero.
! -----------------------------------------------------------------------------
FUNCTION print_formated(input)

	! << Local Variables. >>
	IMPLICIT none
	INTEGER 					 :: print_formated
	DOUBLE PRECISION, INTENT(in) :: input

	! << Specifiy the correct print format given the length of the input. >>
	! an example of '14.95' would be '(F5.2)' because we have 5 total 
	! characters and 2 of these characters are on the left of the decimal.
	IF (input < 10) THEN
		WRITE(*, '(F4.2)') input 
	ELSE IF (input < 100) THEN
		WRITE(*, '(F5.2)') input 
	ELSE IF (input < 1000) THEN
		WRITE(*, '(F6.2)') input
	ELSE IF (input < 10000) THEN
		WRITE(*, '(F7.2)') input  
	ELSE
		WRITE(*, '(F10.2)') input 
	END IF

	! << Set return value. >>
	print_formated = 0

! << End function. >>
END FUNCTION



! -----------------------------------------------------------------------------
! << Start function: get_items >>
!
! This function is used to gather the input value of all items from the user.
! entering the sequence 'end' will exit the prompt and return from the function.
!
! @params -There are no parameters.
! @return -Returns the sub total amount.
! -----------------------------------------------------------------------------
FUNCTION get_items()

	! << Local Variables. >>
	IMPLICIT none
	DOUBLE PRECISION  :: get_items			 ! Function
	DOUBLE PRECISION  :: sub_total       = 0
	DOUBLE PRECISION  :: new_item_value  = 0
	CHARACTER(LEN=10) :: new_item_string
	INTEGER			  :: new_item_stat   = 0
	INTEGER 		  :: print_formated   	 ! Function 

	! << Loop until user is finished. >>
	DO

		! << Prompt the user. >>
    	WRITE(*, '(A)', ADVANCE = "NO") "Enter the price of your item: $"
    	
    	! << Read user input as a string. >>
    	READ(*,*) new_item_string

    	! << Check if user entered the "end" keyword. >>
    	IF (new_item_string .EQ. 'end') THEN

    		PRINT *, ''

    		! << Break the loop. >>
    		EXIT

    	! << User did not enter the "end" keyword. >>
    	ELSE

    		! << Read user input to double variable, use iostat error checking. >>
    		READ(new_item_string,*,iostat=new_item_stat) new_item_value

    		! << Check iostat for error, if none, complete if block. >>
    		IF (new_item_stat == 0) THEN

    			! << Update and print subtotal for the user, attempt correct formating. >>
    			sub_total = sub_total + new_item_value
    			WRITE(*, '(A)', ADVANCE = "NO") "Your sub total is : $"
    			
    			! << Call the formating function, use new item as a temp. >>
			   	new_item_value = print_formated(sub_total)

			   	PRINT *, ''

    		! << Found iostat error, handle exception with message. >>
    		ELSE

    			PRINT *, 'Input Error: ', new_item_string, ' Not a valid price'
    			
    		END IF

    	END IF

    END DO

	! << Set return value. >>
	get_items = sub_total

! << End function. >>
END FUNCTION



! -----------------------------------------------------------------------------
! << Start function: get_change >>
!
! This function is used to promt the user for payment of the total. Displays a
! message for different entries.
!
! @params (DOUBLE PRECISION end_total) -The total amount to be paid.
! @return -Void return, default by setting to zero.
! -----------------------------------------------------------------------------
FUNCTION get_change(end_total)

	IMPLICIT none
	DOUBLE PRECISION   			 :: get_change			! Function
	DOUBLE PRECISION    		 :: end_change = 0
	DOUBLE PRECISION, INTENT(in) :: end_total
	DOUBLE PRECISION    		 :: new_item_value = 0
	CHARACTER(LEN=10)   		 :: new_item_string
	INTEGER			    		 :: new_item_stat = 0
	INTEGER 					 :: print_formated     	! Function 


    ! << Read user input as a string. >>
    READ(*,*) new_item_string

    ! << Read user input to double variable, use iostat error checking. >>
    READ(new_item_string,*,iostat=new_item_stat) new_item_value

    PRINT *, ''

    ! << Check iostat for error, if none, complete if block. >>
    IF (new_item_stat == 0) THEN

    	! << Calculate the change for the sale. >>
    	end_change = new_item_value - end_total

    	! << The customer cant pay for the items. >>
    	IF (end_change < 0) THEN

    		PRINT *, "This is not a charity, go put everything back."
    	
    	! << The Customer provided a valid input amount. Print resulting
    	! change amount, attempt proper formating.
    	ELSE

    		WRITE(*, '(A)', ADVANCE = "NO") "Your change is: $"

    		! << Call the formating function, use new item as a temp. >>
			new_item_value = print_formated(end_change)

    		PRINT *, ''
    		PRINT *, "Have a nice day"

    	END IF

    ! << Found iostat error, handle exception with message. >>
    ELSE

    	PRINT *, "WARNING: Invalid Form of payment!!!"
    	PRINT *, "No items for you, The cops are on the way."

    END IF

	! << Set return value. >>
	get_change = 0

! << End function. >>
END FUNCTION