	.data
length_divisor_prompt:
	.asciiz "Enter space-seperated integers array length n and divisor k: "
array_input_prompt_begin:
	.asciiz "Enter "
array_input_prompt_end:
	.asciiz " space-seperated integers: "
print_arr_begin:
	.asciiz "ar = [ "
print_arr_end:
	.asciiz "]\n"
length_out_msg:
	.asciiz "Length (n): "
divisor_out_msg:
	.asciiz "Divisor (k): "
length_divisor_err_msg:
	.asciiz "2 <= n <= 100 and 1 <= k <= 100 (n: array length, k: divisor)\n"
syntax_err_msg: 
	.asciiz "Invalid syntax. Please make sure to enter space between positive integers\n"
arithmetic_err_msg:
	.asciiz "Each integer value v should be in the boundry 1 <= v <= 100\n"
pair_str:
	.asciiz "Number of pair(s): "
newline:
	.asciiz "\n"
space: 
	.asciiz " "
openp:
	.asciiz "("
closep:
	.asciiz ")"
comma:
	.asciiz ","
buffer:
	.space 64
max_length:
	.word 64
boundry_arr:
	.word 0, 0

	.text
	.globl main
main:

	take_length_divisor:
		li $v0, 4 				# prompt the user
		la $a0, length_divisor_prompt
		syscall
		li $v0, 8  				# get the number of input and the divider (single line, space-between)
		la $a0, buffer
		la $a1, max_length 		# provide max length of the string
		syscall
	
		# parse the line and get the number of item and divider as integer 	
		la $a0, boundry_arr 	# give a static array as argument to put the result  
		li $a1, 2 				# give the length of the static array as argument 
		la $a2, buffer 			# give the user input string as argument 
		jal stoi 				# fills the given array by converting the ints inside buffer
		beq $v0, 1, take_length_divisor # check if there is an error	
		
		la $t0, boundry_arr
		lw $s1, 0($t0) 			# save array length (n) in register $s1
		lw $s2, 4($t0) 			# save the divider (k) in register $s2

		# check if the inputs are suitable for constrain
		# 2 <= n <= 100
		blt $s1, 2, length_divisor_err
		bgt $s1, 100, length_divisor_err
		# 1 <= k <= 100
		blt $s2, 2, length_divisor_err
		bgt $s2, 100, length_divisor_err
		
		
		li $v0, 4 # print the length output message
		la $a0, length_out_msg
		syscall
		li $v0, 1 # print the value of length
		move $a0, $s1
		syscall
		li $v0, 4 # print comma
		la $a0, comma
		syscall
		li $v0, 4 # print space
		la $a0, space
		syscall
		li $v0, 4 # print the divisor output message
		la $a0, divisor_out_msg
		syscall
		li $v0, 1  # print the value of divisor
		move $a0, $s2
		syscall
		li $v0, 4 # print newline
		la $a0, newline
		syscall
		
		j mem_alloc
		
	length_divisor_err:
		li $v0, 4 			# print the error message
		la $a0, length_divisor_err_msg
		syscall
		j take_length_divisor
					
	mem_alloc:
		sll $a0, $s1, 2			# calculate the required memory space for int array (length * 4)
		li $v0, 9 				# make a dynamic memory allocation for creating a array
		syscall
		move $s0, $v0 			# save the start address of array in $s0
		
	take_array:
		li $v0, 4 				# promt the user
		la $a0, array_input_prompt_begin
		syscall
		li $v0, 1				# get the integers
		move $a0, $s1			# array length
		syscall			
		li $v0, 4 				# promt the user
		la $a0, array_input_prompt_end
		syscall	
		li $v0, 8				# get the integers
		la $a0, buffer
		la $a1, max_length 		# provide max length of the string
		syscall
	
		move $a0, $s0       # set the start address of array
		move $a1, $s1 		# set the array length
		la $a2, buffer		# set the string to be parsed
		jal stoi 			# fills the given array by converting the integers from inside the buffer
		beq $v0, 1, take_array # if there is an error occured, then try again taking the integers from user	
		
		# print array
		move $a0, $s0 # put the start adress of array 
		move $a1, $s1 # put the array length
		jal print_arr
		
	find_pairs:
		move $a0, $s0 		# give the start address of the array
		move $a1, $s1 		# give the size of the array
		move $a2, $s2 		# give the divider
		jal divisible_sum_pairs
		move $s0, $v0 		# keep the result in $s0

		li $v0, 4 			# print the result sentence
		la $a0, pair_str
		syscall
		li $v0, 1 			# print the result value
		move $a0, $s0
		syscall
		li $v0, 10 			# terminate the execution
		syscall

stoi:
	# parses the given string, builds integers and place them in given array
	# after array is full or string terminator is encountered the parsing process ends
	# $a0: start address of array
	# $a1: length of the array
	# $a2: start address of string 
	
	addi $sp, $sp, -16 		# adjust stack to make room for 4 items 
	sw $s3, 12($sp) 		# save registers for use afterwards 
	sw $s2, 8($sp)
	sw $s1, 4($sp) 
	sw $s0, 0($sp) 
	
	move $s0, $a0 	# start address of the int array
	move $s1, $a1 	# length of the array
	add $s2, $a2, -1 	# start address minus 1 of the string
	li $s3, -1 		# initial value of the number that is gone be built
	li $t0, 0 		# set the counter
	li $v0, 0 		# set the return value 0 to indicate there is no error initially 
	
	stoi_loop:
		addi $s2, $s2, 1 		# update $s2 as next charachter of the string
		lbu $t1, 0($s2)       	# load unsigned char from string into $t1
		beq $t0, $s1, stop  	# if the array is fully filled (counter == length)
  		beq $t1, $zero, stop    # if char is NULL terminator
  		beq $t1, 10, save  		# if char is new line
  		beq $t1, 32, save 		# if char is space
  		blt $t1, 48, syntax_err   	# if char is not a digit (ascii < '0')
  		bgt $t1, 57, syntax_err   	# if char is not a digit (ascii > '9')
		bne $s3, -1, positive   # generated number is -1 than convert it to 0
		li $s3, 0
	positive:
		addi $t1, $t1, -48   	# converts t1's ascii value to dec value
 		mul $s3, $s3, 10    	# sum *= 10
  		add $s3, $s3, $t1    	# sum += array[s1] - '0'
		# addi $s2, $s2, 1 		# update $s2 as next charachter of the string
		j stoi_loop
	
	save:
		# if the current charachter is space and the generated value is 0 then just continue looping
		# beq $t1, 32, save 		# if char is space
		
		beq $s3, -1, stoi_loop 	# leading space encountred
		
		# check if the generated value is suitable for constrain 1 <= arr[i] <= 100		
		blt $s3, 1, arithmetic_err
		bgt $s3, 100, arithmetic_err
		
		sw $s3, 0($s0) 		# save the builded number inside array
		li $s3, -1 			# reset the value of the build number
		addi $s0, $s0, 4    # increment the array address for next storage
		# addi $s2, $s2, 1  	# update $s2 as next charachter of the string
		addi $t0, $t0 1 	# increment the counter inside the array
		j stoi_loop
	
	syntax_err:
		li $v0, 4 			# print the error message
		la $a0, syntax_err_msg
		syscall
		li $v0, 1 # set the return value 1 to indicate execution failed
		j stop
		
	arithmetic_err:
		li $v0, 4 			# print the error message
		la $a0, arithmetic_err_msg
		syscall
		li $v0, 1 # set the return value 1 to indicate execution failed
		j stop

	stop:
		lw $s3, 12($sp) 	# restore registers for caller
		lw $s2, 8($sp) 
		lw $s1, 4($sp) 
		lw $s0, 0($sp) 
		addi $sp, $sp, 16 	# adjust back to stack to delete 4 items
		jr $ra

divisible_sum_pairs:
	# finds the divisible sums pair(s) (i < j && ((arr[i] + arr[j]) % d == 0))
	# $a0: start address of array
	# $a1: array length
	# $a1: divider
	
	# $s0: end address 
	# $s1: address of list[i] 
	# $s2: address of list[j] 
	# $s3: divider
	# $s4: pair counter
	
	addi $sp, $sp, -20 	# adjust stack to make room for 4 items 
	sw $s4, 16($sp) 		# save registers for use afterwards 
	sw $s3, 12($sp)
	sw $s2, 8($sp)
	sw $s1, 4($sp) 
	sw $s0, 0($sp) 
	
	sll $s0, $a1, 2 		# calculate memory space (length * 4)
	add $s0, $a0, $s0  		# end address = start address + total space
	move $s3, $a2 			# keep divider inside $s3
	li $s4, 0 				# initialize the counter
	move $s1, $a0 			# set iterator of outer loop
	
	iloop:
		beq $s1, $s0, iexit
		addi $s2, $s1, 4 		# set start address for inner loop (j = i + 1)
		lw $t1, 0($s1) 			# get the value list[i]
		jloop:
			beq $s2, $s0, jexit
			lw $t2, 0($s2) 		# get the value list[j]
			
			add $t3, $t1, $t2 	# list[i] + list[j]	
			div $t3, $s3 	 	# apply division and get the remainder from $hi 
			mfhi $t3
			beq $t3, $zero, pair_found
			addi $s2, $s2, 4 	# switch the next memory position (++J)
			j jloop
			
		pair_found:
			addi $s4, $s4, 1 	# increase the counter
			addi $s2, $s2, 4 	# iterate the inner loop
			move $a0, $t1 		# set the procedure parameters
			move $a1, $t2
			move $s5, $ra 		# save the previous return address before jump and link
			jal print_pair
			move $ra, $s5  		# set the previous return address 
			j jloop
		
		jexit:
			addi $s1, $s1, 4  	# switch the next memory position (++i)
			j iloop
	
	iexit:
		move $v0, $s4 			# return the counter value as total number of pair(s)
		addi $sp, $sp, 20 		# adjust back the stack 
		lw $s4, 16($sp) 		 
		lw $s3, 12($sp)
		lw $s2, 8($sp)
		lw $s1, 4($sp) 
		lw $s0, 0($sp) 
		jr $ra

print_pair:
# prints integer pair (x,y)
# $a0: first integer
# $a1: second integer

	addi $sp $sp -8 # make room in the stack
	sw $s0, 0($sp)
	sw $s1, 4($sp)

	move $s0, $a0 	# keep first value in $s0
	move $s1, $a1 	# keep second value in $s1
	
	li $v0, 4 		# print open paranthesis
	la $a0, openp
	syscall
	
	li $v0, 1 		# print first integer
	move $a0, $s0
	syscall
	
	li $v0, 4 		# print comma
	la $a0, comma
	syscall
	
	li $v0, 4 		# print space
	la $a0, space
	syscall
	
	li $v0, 1 		# print second integer
	move $a0, $s1 
	syscall
	
	li $v0, 4 		# print closed paranthesis
	la $a0, closep
	syscall
	
	li $v0, 4 		# print new line charachter
	la $a0, newline
	syscall
	
	lw $s0, 0($sp)
	lw $s1, 4($sp)
	addi $sp $sp 8 	# adjust back the stack
	jr $ra

print_arr:
	# prints integer pair (x,y)
	# $a0: start address of the array
	# $a1: length of the array

	addi $sp, $sp, -16 	# adjust stack to make room for 4 items 
	sw $s3, 12($sp) 		# save registers for use afterwards 
	sw $s2, 8($sp)
	sw $s1, 4($sp) 
	sw $s0, 0($sp) 

	move $s0, $a0 # keep the array start address
	move $s1, $a1 # keep the length of the array 
	li $s2, 0 # keep the counter

	
	li $v0, 4			# print arr start sentence
	la $a0, print_arr_begin
	syscall

	print_loop:
		beq $s2, $s1, print_loop_exit 		# make sure to not exceed array length
		li $v0, 1		 	# print the integer value
		lw $a0, 0($s0)
		syscall
		li $v0, 4			# print space
		la $a0, space
		syscall
		addi $s0, $s0, 4 # iterate the next memory location
		addi $s2, $s2, 1 # increase the counter
		j print_loop
	
	print_loop_exit:		
		li $v0, 4			# print end sentence
		la $a0, print_arr_end
		syscall
	
		lw $s3, 12($sp) 	# restore registers for caller
		lw $s2, 8($sp) 
		lw $s1, 4($sp) 
		lw $s0, 0($sp) 
		addi $sp, $sp, 16 	# adjust back to stack to delete 4 items
		jr $ra
