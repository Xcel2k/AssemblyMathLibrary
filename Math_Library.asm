;Code by Muhammad Osama cms 36270 NBC CS-2020
;Course Code: 235 Computer Organization and Assembly Language
;Instructor: Mr. Najeebullah

;=============================================================================
;NOTE: Please note that I have used the basic knowledge of maths that i had.
;i'm nowhere near as good in maths as other students might be, but I tried
;as per my capabilites, some functions might be goofy but in my own head they
;made perfect sense. Thank you!
;=============================================================================

[org 0x0100] 
 


jmp start_of_code
 
 x   	: dd 2
num  	: dd 0
perp 	: dd 0
base 	: dd 0
a    	: dd 3
b    	: dd 2
res  	: dd 0
msg  	: dw ''
var1	: dd 1.0,2.0,3.0,4.0
var2	: dd 5.0,6.0,7.0,8.0

;declarations for floor and ceil
var3 	: dd 10.0
infin	: dd -0.5
var4	: dw 0

;declaration for 20th function read
buff	: dw 100
blen	: equ $-buff

;declaration for 25th function
entered : db ''

;declarations for 27th function "Copying String"
string1 : db ''
string2 : db ''		;string2 is just a buffer 

;declaration for 21st and 22nd funtions which are upper case and lowercase
data:
string	: dw ''
;------------------------------------------------------------------
; 1st Function "Sin"
;------------------------------------------------------------------

start_of_code:

sin:                     ;√(1-x^2)
      push bp
      mov bp,sp
      push dword [x]
      pusha              ;push all general purpose registers
      mov cx,1              
      mov bx,[x]         ;moving the value of x into bx register
      mov dx,[x]         ;moving the value of x into dx register to take square
      mul dx             ; x^2  is moved in dx
      sub cx,dx          ;1-x^2
      call isqrt         ;√1-x^2
      popa               ;pop all general purpose registers
      pop dword [x]
      mov sp,bp
      pop bp
      ret
;---------------------------------------------------------------
; 2nd Function "Cos" 
;---------------------------------------------------------------
cos:      
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all general purpose registers
                 
       mov cx,1
       mov bx,[x]
       mov dx,[x]        ;moving the value of x into dx register to take square
       mul dx            ; x^2  is moved in dx
       sub cx,dx         ;1-x^2
       call isqrt        ;√1-x^2
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;---------------------------------------------------------------
; 3rd Function "Tan"
;---------------------------------------------------------------
tan:                    ;1+x^2
       push bp
       mov bp,sp
       push dword [x]
       pusha            ;push all gprs
       mov cx,1
       mov bx,[x]
       mov dx,[x]
       mul dx           ;x^2
       add cx,dx        ;1+x^2
       popa             ;pop all general purpose registers
       pop dword [x] 
       mov sp,bp
       pop bp
       ret
;---------------------------------------------------------------
; 4th Function "arcsin" which is also called asin or sin^-1
;---------------------------------------------------------------
arcsin:                  ;1/√(1-x^2)
      push bp
      mov bp,sp
      push dword [x]
      pusha              ;push all general purpose registers
      mov cx,1              
      mov bx,[x]         ;moving the value of x into bx register
      mov dx,[x]         ;moving the value of x into dx register to take square
      mul dx             ; x^2  is moved in dx
      sub cx,dx          ;1-x^2

      call isqrt         ;√1-x^2
      
      mov ax,1 
      div cx             ;1/√(1-x^2)
      mov ax,cx          ;mov the result back in ax
      popa               ;pop all general purpose registers
      pop dword [x]
      mov sp,bp
      pop bp
      ret
;--------------------------------------------------------------
; 5th Function "arccos" which is also called acos or cos^-1
;--------------------------------------------------------------
arccos:                  ;-1/√(1-x^2)
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all general purpose registers
                 
       mov cx,1
       mov bx,[x]
       mov dx,[x]        ;moving the value of x into dx register to take square
       mul dx            ; x^2  is moved in dx
       sub cx,dx         ;1-x^2
       call isqrt        ;√1-x^2
       mov ax,-1 
       div cx            ;-1/sqrt(1-x^2)
       mov ax,cx         ;mov the result back in ax
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;---------------------------------------------------------------
; 6th Function "arctan" which is also called atan or tan^-1
;---------------------------------------------------------------
arctan:                  ;1/1+x^2
       push bp
       mov bp,sp
       push dword [x]
       pusha             ;push all gprs
       mov cx,1
       mov bx,[x]
       mov dx,[x]
       mul dx            ;x^2
       add cx,dx        ;1+x^2
       mov ax,1
       div cx            ;1/1+bx^2
       mov ax,cx         ;move the result in ax
       popa              ;pop all general purpose registers
       pop dword [x]
       mov sp,bp
       pop bp
       ret
;---------------------------------------------------------------
; 7th Function "hypotenuse" 
;---------------------------------------------------------------
hyp:                    ; hyp=√perp^2+base^2
       push bp
       mov bp,sp
       push dword [perp]
       push dword [base]
       pusha  
       mov ax,[perp]
       mov bx,[perp]  	 ;moving perp in ax and bx registers for taking square
       mul bx           ;perp^2 in bx
       mov cx,[base]
       mov dx,[base] 
       mul dx           ;base^2 in dx
       add bx,dx        ;perp^2+base^2
       call isqrt       ;√perp^2+base^2
       mov ax,bx        ;store the result back in ax
       popa             ;pop all general purpose registers
       pop dword [base]
       pop dword [perp]
       mov sp,bp
       pop bp
       ret
;---------------------------------------------------------------
; 8th Function "atan2" which is also called tan^-1(2) this divides
;two numbers given to the function and takes the atan of them
;---------------------------------------------------------------
atan2: 
	push bp
	mov bp,sp
	push dword [x]
	push dword [a]
	push dword [b]
	pusha
	mov ax,[a]		;moving a into ax
	mov bx,[b]		;moving b into bx
	div bx			;dividing ax by bx i.e., a/b 
	mov [res],bx		;moving the result to the res variable
	call arctan		;applying the arctan function or the tan^-1 function on res
	popa
	pop dword [b]
	pop dword [a]
	pop dword [x]
	mov sp,bp
	pop bp
	ret
	
;---------------------------------------------------------------
; 9th Function "sqrt" using the binary reducing algorithm
;---------------------------------------------------------------

;i also tried using the PEXT command to extract bits from the source register which are 
;also present in the mask register and placing the value in the destination register 
;i.e.,  mov cx, 0x55555555   where odd-numbered bits = 0, even-numbered bits = 1
;	pext dx, bx, cx  extract the even-numbered bits of ax into cx
;but my processor doesnt support the pext command, i tried the sse4 alternative of the pext
;command which is "extrq" but i just couldnt manage the syntax.

;logic which i used in the trial of this above explanation was from geeksforgeeks
;link is as follows: 
;https://www.geeksforgeeks.org/swap-all-odd-and-even-bits/

;---------------------------------------------------------------
isqrt:

        mov ax, [num]
        xor bx, bx
        bsr cx, ax		;bit scan reverse instruction, finds the msb of the input set 
        			;and removes it so we can add it to the latter of the input 
        			;at the end, in simple words, extraction of msb
        and cl, 0feh		;254 in hexa because we have extracted the msb already so we are 
        			;adding the remaining bits 0-255 or in this case now 0-254 to the 
        			;lower part of the counter register
        mov dx, 1
        shl dx, cl             	;shift left 
processing:
        mov si, bx
        add si, dx
        cmp si, ax
        ja msbremoval		;again, extracting msb 
        sub ax, si
        
        shr bx, 1
        add bx, dx                                                                                                             
        jmp approxsqrt
msbremoval:
        
        shr bx, 1

approxsqrt:
        
        shr dx, 2
        
        jnz processing
        mov ax, bx
        ret
;-------------------------------------------------------------
; 10th Function "power"
;-------------------------------------------------------------
power:
        push bp
        mov bp,sp
        sub sp,4
        mov bx,[bp+8]  ;put the first element in bx that is the base
        mov cx,[bp+12] ;put the second element in cx that is the exponent
        mov [bp-4],bx  ;store the current result
 
working_loop:
        cmp cx,1       ;if the power is one return
        je end
        mov ax,[bp-4]  ;move the result in ax
        imul ax,bx     ;multiply the current result by the base number
        mov [bp-4],ax  ;store the current result
        dec cx
        jmp working_loop
 
end:
       mov ax,[bp-4]
       mov sp,bp
       pop bp
       ret
  
;---------------------------------------------------------------
; 11th Function "printstring" 
;---------------------------------------------------------------
printstring:
	push bp
	mov bp,sp
	push word [msg]
       
	pusha
	mov dx,msg	;move the msg in dx            
	call length
	mov bx,1
	mov ah,0x40	;write
	int 0x21	;interupt call
        popa
        
        pop word [msg]
        popa
        mov sp,bp
        pop bp
        ret
;-----------------------------------------------------------------
; 12th Function "int absolute" 
;-----------------------------------------------------------------
 iabs:
 	push bp
 	mov bp,sp
 
        push ax     
        push dx  
           		;cdq copies the sign of the register ax to register dx
           		;For example, if it is a positive number, dx will be zero, otherwise, 
        cdq         	;dx will be 0xFFFF which denotes -1. The xor operation with the origin
        xor ax, dx  	;number will change nothing if it is a positive number 
        		;xor wont have any effect
        sub ax, dx  	
        pop dx
        pop ax
        mov sp,bp
  
        pop bp
        ret
;------------------------------------------------------------------
; 13th Function "Natural Log" 
;------------------------------------------------------------------
log: 				;1/x i.e., formula of natural log 
	push bp
	mov bp,sp

	push dword [x]
	pusha
	
	mov cx,1		;moving 1 to cx
	mov ax,[x]		;moving x to ax
	
	div ax			;dividing cx by ax i.e., 1/x
	mov dx,ax 		;moving the result to dx
	popa 
	pop dword [x]
	mov sp,bp
	pop bp
	ret	

;---------------------------------------------------------------------
; 14th Function "fabs" which is also called the floating point abs
;---------------------------------------------------------------------

;since we are making a floating point function, it might serve beneficial for me 
;if i were to explain a little bit of what i am trying to achieve
;because the functions above were a lot simpler and required only a basic knowledge of maths and 
;general arithmetic rules of assembly


;---------------------------------------------------------------------

;i will be using the movups command which means move unaligned packed singles
;i could have also used the movaps command which just means moving aligned packed singles 
;but i wanted my code to be general, and since we're always going back to declare new variables in our code
;i felt that movups was the better command to opt for
;moreover, i am using the destructive instruction set for this function i.e., the sse instruction set (sse2)

;---------------------------------------------------------------------
floatingabs: 

	push bp
	mov bp,sp
	
	pusha 
	
	movups xmm0,[var1]
	movups xmm1,[var2]
	
	
	;normally the convention for using xmm registers and movups commands is movups xmm1,xmm2/m128 but for some reason
	;it was giving an invalid combination of opcode and operand error so by adding square brackets t omy variable instead of using a xmmword ptr 
	;it solves the problem, just in case you were wondering
	
	
	cqo				;cqo instructio is used to produce a double quadword dividend from a quadword before a quadword division
	xorps xmm0,xmm1			;bitwise logical xor operation (from page # 2537 of the intel ref manual)
	subps xmm1,xmm0
	
	popa
	
	mov sp,bp
	pop bp
	ret
	
;---------------------------------------------------------------------
; 15th Function "Length of String"
;---------------------------------------------------------------------

length: 
	push bp 
 	mov bp,sp 
 	push es 
 	push cx 
 	push di 
 	les di, [bp+4]		;point es:di to string 
 	mov cx, 0xffff		;load maximum number in cx 
 	xor al, al 		;load a zero in al 
 	repne scasb 		;find zero in the string, saw this instruction in the handouts that were provided
 	mov ax, 0xffff		;load maximum number in ax 
 	sub ax, cx 		;find change in cx 
 	dec ax 			;exclude null from length 
 	pop di 
 	pop cx 
 	pop es 
 	mov sp,bp
 	pop bp 
 	ret 4
;---------------------------------------------------------------------
; 16th Function "Floor" 
;---------------------------------------------------------------------

floor: 
	push bp
	mov bp,sp
	pusha
	
	fld dword [var3]	;loading floating point value into floating point unit register st
				;pg#964 of intel ref manual
	fadd st1,st0
	fadd dword [infin]
	fistp word [var4]	;storing integer pg# 959 of intel ref manual
	sar word [var4],1

	popa
	mov sp,bp
	pop bp
	ret
	

;---------------------------------------------------------------------
; 17th Function "Ceil" 
;---------------------------------------------------------------------

ceil: 
	push bp 
	mov bp,sp
	pusha
	
	fld dword [var3]
	fadd st1,st0
	fsubr dword [infin] 	;reverse subtracting to make the ceiling possible
				;because of logic from this link https://www.geeksforgeeks.org/find-ceil-ab-without-using-ceil-function/
	fistp word [var4]
	sar word [var4],1
	neg word [var4]
	popa
	mov sp,bp
	pop bp
	ret


;---------------------------------------------------------------------
; 18th Function "Random"
;---------------------------------------------------------------------

randnum:
	push bp
	mov bp,sp
	pusha
	
	mov ah,00h
	int 1ah	;interupt to fetch system's time 
	mov ax,dx
	xor dx,dx
	mov cx,100
	div cx 	;cx contains values from 0 to 99
	
	add dl,30h
	
	mov ah,2h	;start of text
	int 21h
	
	popa
	mov sp,bp
	pop bp
	ret


;------------------------------------------------------------------
; 19th Function "Is Equal"
;------------------------------------------------------------------

isequal: 
	push bp 
 	mov bp,sp 
 	push cx 
 	push si 
 	push di 
 	push es 
 	push ds 
 	lds si, [bp+4] 	;point ds:si to first string 
 	les di, [bp+8] 	;point es:di to second string 
 	push ds 		;push segment of first string 
 	push si 		;push offset of first string 
 	call length 		;calculate string length 
 	mov cx, ax 		;save length in cx 
 	push es 		;push segment of second string 
 	push di 		;push offset of second string 
 	call length 		;calculate string length 
 	cmp cx, ax	 	;compare length of both strings 
 	jne exitfalse 		;return 0 if they are unequal 
 
 	mov ax, 1 		;store 1 in ax to be returned 
 	repe cmpsb 		;compare both strings 
 	jcxz exitsimple 	;are they successfully compared 
exitfalse: 
	mov ax, 0 		;store 0 to mark unequal 
exitsimple: 
	pop ds 
	pop es 
	pop di 
 	pop si 
 	pop cx 
 	pop bp 
 	ret 8


;------------------------------------------------------------------
; 20th Function "Read"
;------------------------------------------------------------------

read: 
	push bp
	mov bp,sp
	pusha
	xor ax, ax
	xor di, di      
	mov si,buff    	;buff is just an array  
	mov bx,blen      
	int 21h
	popa
	mov sp,bp
	pop bp
	ret
	
;------------------------------------------------------------------
; 21st Function "Upper case" 
;------------------------------------------------------------------
start:
       push bp
       mov bp,sp
       push word [string]
       pusha         
       mov ax, data
       mov ds, ax          
upper_case: 
       mov bx,string
       cmp [bx],  byte '$'		;looping condition for the whole of string i.e., the length of string
					;signified by the dollar sign
       je done
       
       cmp byte  [bx], 'a'
       jb next
       cmp byte [bx], 'z'
       ja next
       and byte  [bx], 11011111b	;uppercase mask bit 11011111b
next:	
       inc bx 				;next char
       jmp upper_case   
done:
       lea dx, [string]
       mov ah, 09h			;output message, taken from the link: http://www.husseinsspace.com/teaching/udw/1996/asmnotes/chapone.htm
       int 21h
       popa
       pop word [string]
       mov sp,bp
       ret
       
;------------------------------------------------------------------
; 22nd Function "Lower case" 
;------------------------------------------------------------------       
start_2:
       push bp
       mov bp,sp
       push word [string]
       pusha         
       mov ax, data
       mov ds, ax          
lower_case: 
       mov bx,string
       cmp [bx],  byte '$'
       je done
       
       cmp byte  [bx], 'A'
       jb next
       cmp byte [bx], 'Z'
       ja next
       add byte  [bx], 20h		;we will add 20 ascii characters to achieve lowercasing of the given string
next_2:	
       inc bx 				;next char
       jmp lower_case   
done_2:
       lea dx, [string]
       mov ah, 09h
       int 21h
       popa
       pop word [string]
       mov sp,bp
       ret
       
;------------------------------------------------------------------
; 23rd Function "Cosh" which is the cosine hyperbolic function 
;------------------------------------------------------------------    

; for the hyperbolic functions i have used the followin formulas 
; from the link: https://www.facebook.com/102635374851056/posts/hyperbolic-functions/168393921608534/

;------------------------------------------------------------------ 


cosh: 
	push bp
	mov bp,sp
	push dword [x]
	pusha
	
	mov ax,[x]
	mov bx,-1		;loading -1 in bx
	call isqrt		;taking square root of -1, which is iota
	imul ax			;multiplying iota with the value of x
	call cos		;cos of iota and value of x gives us cosh function
	
	popa
	pop dword [x]
	mov sp,bp
	popbp
	ret	

;------------------------------------------------------------------
; 24th Function "Tanh" which is the tangent hyperbolic function 
;------------------------------------------------------------------ 


tanh:
	push bp
	mov bp,sp
	push dword [x]
	pusha 
	
	mov ax,[x]
	mov bx,-1		;loading -1 in bx
	call isqrt		;taking square root of -1 which is iota
	imul ax			;multiplying iota with the value of x
	neg ax			;making the sign negative since it is in the formula as per the link i have
				;attached above 
	call tan		;tan of iota and value of x whole negative gives us tanh function
	
	popa
	pop dword [x]
	mov sp,bp
	pop bp
	ret
	
;------------------------------------------------------------------
; 25th Function "String to Int"  
;------------------------------------------------------------------ 
	mov dx, entered 	;our string
initial:
	xor ax, ax 		;zero a "result so far"
top_3:
	movzx si,byte [di]	;get a character
	inc di			;ready for next one
	cmp cx, '0' 		;checking the validity
	jb done_3
	cmp cx, '9'
	ja done_3
	sub cx, '0' 		;"convert" character to number
	imul ax, 10 		;multiply "result so far" by ten
	add ax, cx 		;add in current digit
	jmp top_3 ; until done
done_3:
	ret


;------------------------------------------------------------------
; 26th Function "Int to String"  
;------------------------------------------------------------------ 
itoas:					;integer to a string
	push dx
	add	si,6			;advance to end of buffer
	push	si			;and save that address.
	or	ax,ax			;test sign of 16-bit value,
	pushf				;and save sign on stack.
	jns	itoas1			;jump if value was positive.
	neg	ax			;find absolute value.

itoas1:	
	mov	dx,0			;divide value by radix to extract
	div	cx			;next digit for forming string
	add	dl,'0'			;convert remainder to ASCII digit
	cmp	dl,'9'			;in case converting to hex ASCII,
	jle	itoas2			;jump if in range 0-9,
	add	dl,'A'-'9'-1		;correct digit if in range A-F

itoas2:	
	dec	si			;back up through buffer
	mov	[si],dl 		;store this character into string
	or	ax,ax
	jnz	itoas1			;if not then convert another digit

	popf				;if the original value is negative then pop and return +ve to stack using pg# 1587 of ref manual
	jns	itoas3			;if not then jump, else the following command will be executed
	dec	si			;store sign into output
	mov	byte [si],'-'

itoas3:	
	pop	ax			;calculate length of string
	sub	ax,si
	pop	dx
	ret				;back to caller

;------------------------------------------------------------------
; 27th Function "Copying String"  
;------------------------------------------------------------------ 

copyingstring:
	push bp
	mov bp,sp
	pusha
	
	
	mov ax,[string1]		;moving string 1 into ax 
	mov bx,[string2]		;moving string2 into bx
	mov dx,ax			;for copying purposes, transfer ax to dx, which means string1 will be moved to dx
	mov cx,bx			;same for string2 and cx
	xchg ax,bx			;we swap the strings 
	xor dx,dx			;and redefine the dx and cx registers with 0
	xor cx,cx
	
	
	popa
	mov sp,bp
	pop bp
	ret

exit_of_code:
	mov ax,4c00h
	int 21h

