title   ***CHIP-8 INTERPRETER***
comment author: Bojan Sofronievski
.386
.model small

;-------------------------------------------------
; Stack segment
;-------------------------------------------------
stack_seg   segment stack 'stack'
    db  100 dup(?)
stack_seg   ends

;-------------------------------------------------
; Data segment
;-------------------------------------------------
data_seg    segment use16   'DATA'
    mem         db      4096    dup(0)                      ; 4 kB RAM
    PC          dw      0                                   ; 16-bit program counter - points to the *currently* executing instruction
    V           db      16      dup(0)                      ; 16 8-bit general purpose (V0 to VF)
    I           dw      0                                   ; 16-bit index register
    
    stk         dw      16      dup(0)                      ; the stack which contains 16 16-bit values
    stk_p       dw      0                                   ; pointer to the next free item on the stack
    
    display     db      2048    dup(0)                      ; buffer for the monochrome screen, resolution 64x32 pixels
    disp_flag   db      0                                   ; flag indicating when the buffer for screen has changed, to redraw the screen
                                                            ; fontset should be preloaded in the memory, on address 000h
    fontset     db          0F0h, 090h, 090h, 090h, 0F0h    ; sprite for digit 0
                db          020h, 060h, 020h, 020h, 070h    ; sprite for digit 1
                db          0F0h, 010h, 0F0h, 080h, 0F0h    ; sprite for digit 2
                db          0F0h, 010h, 0F0h, 010h, 0F0h    ; sprite for digit 3
                db          090h, 090h, 0F0h, 010h, 010h    ; sprite for digit 4
                db          0F0h, 080h, 0F0h, 010h, 0F0h    ; sprite for digit 5
                db          0F0h, 080h, 0F0h, 090h, 0F0h    ; sprite for digit 6
                db          0F0h, 010h, 020h, 040h, 040h    ; sprite for digit 7
                db          0F0h, 090h, 0F0h, 090h, 0F0h    ; sprite for digit 8
                db          0F0h, 090h, 0F0h, 010h, 0F0h    ; sprite for digit 9
                db          0F0h, 090h, 0F0h, 090h, 090h    ; sprite for digit A
                db          0E0h, 090h, 0E0h, 090h, 0E0h    ; sprite for digit B
                db          0F0h, 080h, 080h, 080h, 0F0h    ; sprite for digit C
                db          0E0h, 090h, 090h, 090h, 0E0h    ; sprite for digit D
                db          0F0h, 080h, 0F0h, 080h, 0F0h    ; sprite for digit E
                db          0F0h, 080h, 0F0h, 080h, 080h    ; sprite for digit F
    
    keys        db      16      dup(0)                      ; state of the 16 buttons (hexadecimal keyboard) which are used for input
                                                            ; 0 - button is not pressed, 1 - button is pressed
                                                            ; mapping the keys from the hexadecimal keyboard to keys of a regular QWERTY keyboard
                                                            ;     hex                           keyboard
                                                            ;   1 2 3 C                         1 2 3 4
                                                            ;   4 5 6 D         -----\          Q W E R
                                                            ;   7 8 9 E         -----/          A S D F
                                                            ;   A 0 B F                         Z X C V
    
    delay_t     db      0                                   ; delay timer, used for events' timing
    sound_t     db      0                                   ; sound timer, used for creating *beep* sound while greater than zero
    beep_f      dw      1193180 / 440                       ; value for the counter 2, so it counts with frequency 440 Hz (1193180 / 440 = 2712)
    beep_flag   db      0                                   ; flag set by an instruction that loads sound_t, indicating start of the *beep*
    
    running     db      0                                   ; flag which is set while the interpreter runs, cleared otherwise

    mem_size        equ     4096                            ; RAM size, in bytes
    fontset_size    equ     80                              ; fontset size, in bytes
    mem_prog_data   equ     3584                            ; RAM size allowed for programs and data, in bytes
    stk_size        equ     16                              ; stack size, in words (2 bytes)
    keypad_size     equ     16                              ; number of keys on the hexadecimal keyboard
    display_width   equ     64                              ; interpreter screen width, in pixels
    display_height  equ     32                              ; interpreter screen height, in pixels
    display_size    equ     2048                            ; total number of pixels on the screen
    pixel_size      equ     5                               ; 5 pixels from the computer screen are 1 pixel for the interpreter
    screen_width    equ     320                             ; computer screen width (corresponding to the set video mode)
    screen_height   equ     200                             ; computer screen height (corresponding to the set video mode)
    pixel_color     equ     48                              ; color for the "white" pixel
    
    opcode      dw      0                                   ; the current opcode
    
    inst_pf     db      10                                  ; number of instructions executing in one frame
    
    rand        db      0                                   ; pseudo-random number
    shift_reg   db      4   dup(0)                          ; 32-bit shift register used by the pseudo-random number generator which uses the LFSR algorithm
    
    fname       db      10  dup(0)                          ; file name of the program
    fhandle     dw      0
    fsize       dw      0                                   ; size of the file
    
    video_mode   db     0                                   ; for storing the old video mode
    
    opcode_jmptable         dw      opcode_0uuu             ; jump table for opcode type
                            dw      opcode_1nnn
                            dw      opcode_2nnn
                            dw      opcode_3xkk
                            dw      opcode_4xkk
                            dw      opcode_5xy0
                            dw      opcode_6xkk
                            dw      opcode_7xkk
                            dw      opcode_8xyu
                            dw      opcode_9xy0
                            dw      opcode_Annn
                            dw      opcode_Bnnn
                            dw      opcode_Cxkk
                            dw      opcode_Dxyn
                            dw      opcode_Exuu
                            dw      opcode_Fxuu

    opcode_8xyu_jmptable    dw      opcode_8xy0             ; jump table for opcode_8xyu
                            dw      opcode_8xy1
                            dw      opcode_8xy2
                            dw      opcode_8xy3
                            dw      opcode_8xy4
                            dw      opcode_8xy5
                            dw      opcode_8xy6
                            dw      opcode_8xy7
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_8xyE
                            dw      opcode_invalid

    opcode_Fxuu_jmptable    dw      opcode_invalid          ; jump table for opcode_Fxuu
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_Fx33
                            dw      opcode_invalid
                            dw      opcode_Fxu5
                            dw      opcode_invalid
                            dw      opcode_Fx07
                            dw      opcode_Fx18
                            dw      opcode_Fx29
                            dw      opcode_Fx0A
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_invalid
                            dw      opcode_Fx1E
                            dw      opcode_invalid
    
    int1c_bx    dw      0                                   ; for storing the old vector for interrupt 1Ch
    int1c_es    dw      0
                                                            ; buffer for string input through stdin
    in_buffer   db      10                                  ; max number of characters that can be entered
                db      ?                                   ; number of characters entered by the user (excluding Return key)
                db      10  dup(0)                          ; the entered string
                                                            
                                                            ; Messages for outputing on stdout
    msg_info    db      "Chip-8 interpreter", 0Ah, 0Dh, "$"
    
    msg_fname   db      "Enter ROM name (must be in same "
                db      "directory with the interpreter):"
                db      0Ah, 0Dh, "$"
                
    err_file    db      0Ah 
                db      "The ROM file could not be found."
                db      0Ah, 0Dh, "$"
                
    err_size    db      0Ah
                db      "The ROM file must not be greater "
                db      "than 4 kB.", 0Ah, 0Dh, "$"
    
    frame_last  dw      0
    
data_seg    ends

;-------------------------------------------------
; Code segment
;-------------------------------------------------
code_seg    segment use16   'CODE'
    assume  cs:code_seg, ds:data_seg
    
start:
    mov     ax, data_seg
    mov     ds, ax
    mov     ax, 0A000h
    mov     es, ax                                          ; es will be used for writing in the video memory
;-------------------------------------------------
; Main procedure
;-------------------------------------------------
main                proc
    lea     dx, msg_info
    call    PrintString
    
    call    GetFileName                                     ; get the name of the ROM file from stdin

    lea     dx, fname
    xor     al, al                                          ; open the ROM file, in read mode
    call    OpenFile
    jc      error_file
    
    mov     [fhandle], ax
    mov     bx, ax
    
    call    Init                                            ; initialize the interpreter
    call    LoadMemory                                      ; read the file in the memory of the interpreter
    jc      size_error
    
    call    CloseFile                                       ; close the file
    
    call    InitRandom                                      ; seed for the pseud-random number generator
    
    call    InitVideo                                       ; initialize the video mode
    
    call    TimersInit                                      ; initialize the timers
    
    mov     ah, 00h
    int     1Ah
    mov     word ptr [frame_last], dx
    
interpreter:                                                ; main interpreter's loop
    mov     ah, 00h
    int     1Ah
    mov     ax, dx
    sub     ax, [frame_last]
    cmp     ax, 1
    jb      interpreter
    movzx   cx, byte ptr [inst_pf]
instruction:
    call    ExecuteCycle                                    ; execute one instruction
    loop    instruction
    call    DrawScreen                                      ; draw the screen
    call    KeyState                                        ; get the state of the buttons
    mov     word ptr [frame_last], dx
    cmp     byte ptr [running], 1                           ; if Escape is pressed, set running to 0
    je      interpreter
    
    jmp     restore_state
    
error_file:                                                 ; nonexistent file specified, or file that could not be read
    lea     dx, err_file
    call    PrintString
    jmp     main_end
size_error:                                                 ; the ROM file has size greater than the max allowed one
    lea     dx, err_size
    call    PrintString
    call    CloseFile                                       ; close the file
    jmp     main_end
restore_state:
    call    RestoreVideo                                    ; restore the old video mode
    call    TimersRestore                                   ; restore the old timer 0 state
main_end:
    nop
main                endp

;-------------------------------------------------
; Return with status 0
;-------------------------------------------------
mov     ax, 4c00h
int     21h
;-------------------------------------------------

;-------------------------------------------------
; Init procedure
; The procedure initializes and resets the state
; of the interpreter
; 1. Clearing the memory
; 2. Reading fontset into memory
; 3. Setting PC to address 200h
; 4. Clearing the registers (Vx and I)
; 5. Clearing the stack and the stack pointer
; 6. Reseting the timers
; 7. Reseting the state of the buttons
; 8. Clearing the screen buffer
;-------------------------------------------------
Init            proc
    pusha
    push    es
    
    mov     ax, ds                                          ; 1. Clearing the memory
    mov     es, ax
    lea     di, mem
    mov     cx, mem_size
    xor     al, al
    rep     stosb
    
    lea     si, fontset                                     ; 2. Reading fontset into memory
    lea     di, mem
    mov     cx, fontset_size
    rep     movsb
    
    call    Reset                                           ; all other steps
    
    pop     es
    popa
    ret
Init            endp

;-------------------------------------------------
; Reset procedure
; Procedure for reseting the state of the interpreter
; without chaging the memory
; 1. Setting PC to address 200h
; 2. Clearing the registers (Vx and I)
; 3. Clearing the stack and the stack pointer
; 4. Reseting the timers
; 5. Reseting the state of the buttons
; 6. Clearing the screen buffer
;-------------------------------------------------

Reset           proc
    pusha                                                   ; storing the registers on the stack (the actual stack)
    push    es            
    
    mov     ax, 200h                                        ; 1. PC intialization
    mov     [PC], ax
    
    lea     di, V                                           ; 2. Registers intialization (Vx and I)
    mov     cx, 16
    xor     al, al
    rep     stosb
    
    xor     ax, ax
    mov     [I], ax
    
    lea     di, stk                                         ; 3. Clearing the stack and the stack pointer
    mov     cx, stk_size
    xor     ax, ax
    rep     stosw
    
    mov     [stk_p], ax
    
    xor     al, al                                          ; 4. Reseting the timers
    mov     [delay_t], al
    mov     [sound_t], al
    
    call    ResetKeyState                                   ; 5. Reseting the state of the buttons
    
    call    ClearDisplay                                    ; 6. Clearing the screen buffer
    
    mov     byte ptr [running], 1
    
    pop     es                                              ; restoring the registers from the stack
    popa
    ret
Reset           endp

;-------------------------------------------------
; Procedure for clearing the screen buffer
;-------------------------------------------------
ClearDisplay        proc
    push    ax
    push    cx
    push    di
    push    es
    
    mov     ax, ds
    mov     es, ax
    lea     di, [display]
    mov     cx, display_size
    xor     al, al
    rep     stosb                                           ; fill the buffer with zeros
    
    mov     byte ptr [disp_flag], 1                         ; set the display flag
    pop     es
    pop     di
    pop     cx
    pop     ax
    ret
ClearDisplay        endp

;-------------------------------------------------
; Procedure for drawing on the computer screen
; Drawing is done only if disp_flag is set.
; One pixel of the interpreter screen coresponds to
; a square on the computer screen with size 
; pixel_size
;-------------------------------------------------
DrawScreen          proc
    pusha
    
    cmp     byte ptr [disp_flag], 1                         ; check if disp_flag is set
    jne     draw_screen_end                                 ; if not, exit the procedure
    
    xor     dx, dx                                          ; dl stores the current column of the computer screen
                                                            ; dh stores the current row of the computer screen
    lea     bx, display
    mov     si, dx                                          ; for accessing pixel in display
    mov     di, screen_width*(screen_height-pixel_size*display_height)/2 - screen_width
    mov     al, pixel_color
    mov     cx, screen_width                                ; frame around the screen
    rep     stosb
draw_column:
    mov     ah, pixel_size
    push    di                                              ; store the current computer screen location
    
    xor     al, al
    cmp     byte ptr [bx][si], 0
    jz      draw_pixel
    mov     al, pixel_color

draw_pixel:                                                 ; square drawing
    mov     cx, pixel_size
    rep     stosb
    dec     ah
    cmp     ah, 0
    jz      draw_next_column                                ; if the current pixel from display is drawn, go to the next one
    add     di, screen_width - pixel_size                   ; go to next row in video memory
    mov     cx, pixel_size
    jmp     draw_pixel

draw_next_column:
    inc     si
    inc     dl                                              ; increase the column
    cmp     dl, display_width                               ; check if whole row of display has been passed
    je      draw_next_row
    pop     di
    add     di, pixel_size
    jmp     draw_column
    
draw_next_row:
    pop     cx                                              ; to remove the most recently saved di, no need to restore it
                                                            ; because it is already set to the correct location for video memory
    inc     dh                                              ; increase the row counter for display
    cmp     dh, display_height                              ; check if a row of display has been processed
    je      draw_screen_reset_flag
    xor     dl, dl
    jmp     draw_column

draw_screen_reset_flag:
    mov     al, pixel_color
    mov     cx, screen_width                                ; border around the screen
    rep     stosb
    mov     byte ptr [disp_flag], 0                         ; reset disp_flag
draw_screen_end:
    popa
    ret
DrawScreen          endp

;-------------------------------------------------
; Loading the memory, starting on address 200h
; with content of a file
; Input parameters:
; - bx, which contains the file handle
; If the size of the file is greater than the size
; of the maximum memory allowed for program or data,
; the carry flag is set
;-------------------------------------------------
LoadMemory         proc
    push    ax
    push    cx
    push    dx
    
    call    FileSize                                        ; get the size of the file
    mov     cx, ax
    cmp     cx, mem_prog_data
    jg      file_size_error
    
    lea     dx, mem
    add     dx, 200h
    mov     ah, 3Fh
    int     21h                                             ; load the file content in mem, starting on address 200h
    jmp     lm_end
    
file_size_error:
    stc                                                     ; set the carry flag if the size of the file is not appropriate
    
lm_end:
    pop     dx
    pop     cx
    pop     ax
    ret
LoadMemory         endp

;-------------------------------------------------
; Procedure for executing single cycle
; 1. Fetching instruction from memory
; 2. Decoding the instruction
; 3. Executing the instruction
;-------------------------------------------------
ExecuteCycle        proc
    pusha
    push    es
    
    lea     si, mem                                         ; Fetching instruction from memory
    add     si, [PC]
    mov     ax, [si]                                        ; fetching instruction from memory on address stored in PC
    xchg    al, ah                                          ; chip-8 is big-endian, therefore we swap the higher and lower byte from the fetched instruction
    mov     [opcode], ax
    add     word ptr [PC], 2                                ; updating the PC
    
    and     ax, 0F000h                                      ; Instruction decoding and executing
    shr     ax, 11                                          ; shifting for 11 bits (not 12), because the jump table is comprised of words (2 bytes)
    lea     si, opcode_jmptable
    add     si, ax                                          ; si contains the address for the corresponding label in the jump table
    jmp     word ptr [si]
    
opcode_0uuu::
    mov     ax, [opcode]
    and     ax, 0000Fh
    cmp     ax, 00000h
    je      opcode_00E0
    cmp     ax, 0000Eh
    je      opcode_00EE
    jmp     opcode_invalid                                  ; if the interpreter reached this point, the opcode is invalid
    
opcode_00E0:                                                ; opcode 00E0h: Clear the screen
    call    ClearDisplay
    jmp     execute_cycle_end
opcode_00EE:                                                ; opcode 00EEh: Return from a subprocedure
    sub     word ptr [stk_p], 2                             ; decrease the stack pointer
    lea     si, stk
    add     si, [stk_p]
    mov     ax, [si]                                        ; get the address from the top of the stack
    mov     [PC], ax                                        ; and assing it to PC
    jmp     execute_cycle_end
    
opcode_1nnn::                                               ; opcode 1NNNh: Jump to address NNNh
    mov     ax, [opcode]
    and     ax, 0FFFh
    mov     [PC], ax                                        ; load PC with address NNNh
    jmp     execute_cycle_end
    
opcode_2nnn::                                               ; opcode 2NNNh: call subroutine at address NNNh
    lea     si, stk
    add     si, [stk_p]
    mov     ax, [PC]
    mov     [si], ax                                        ; store the PC on stack
    add     word ptr [stk_p], 2                             ; update the stack pointer
    mov     ax, [opcode]
    and     ax, 0FFFh
    mov     [PC], ax                                        ; load PC with the starting address of the subroutine
    jmp     execute_cycle_end
    
opcode_3xkk::                                               ; opcode 3XKKh: skip next instruction, if Vx == KK
    mov     ax, [opcode]
    mov     dl, al                                          ; the constant kk is stored in dl
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si contains x, the number of the register (Vx)
    lea     bx, V
    cmp     [bx][si], dl
    jne     execute_cycle_end                               ; if Vx != kk, increase PC by two
    add     word ptr [PC], 2                                ; if Vx == kk, increase PC by two more (total 4)
    jmp     execute_cycle_end

opcode_4xkk::                                               ; opcode 4XKKh: skip next instruction, if Vx != KK
    mov     ax, [opcode]
    mov     dl, al                                          ; the constant kk is stored in dl
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si contains x, the number of the register (Vx)
    lea     bx, V
    cmp     [bx][si], dl
    je      execute_cycle_end                               ; if Vx == kk, increase PC by two
    add     word ptr [PC], 2                                ; if Vx != kk, increase PC by two more (total 4)
    jmp     execute_cycle_end

opcode_5xy0::                                               ; opcode 5XY0h: skip next instruction, if Vx == Vy
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si contains y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di contains x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl contains Vy
    cmp     [bx][di], dl
    jne     execute_cycle_end                               ; if Vx != Vy, increase PC by two
    add     word ptr [PC], 2                                ; if Vx == Vy, increase PC by two more (total 4)
    jmp     execute_cycle_end

opcode_6xkk::                                               ; opcode 6XKKh: Load Vx with value KK (Vx = kk)
    mov     ax, [opcode]                                    ; the constant is stored in al
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di contains x
    lea     bx, V
    mov     [bx][di], al                                    ; Vx = kk
    jmp     execute_cycle_end

opcode_7xkk::                                               ; opcode 7Xkkh: Add kk to Vx (Vx = Vx + kk)
    mov     ax, [opcode]                                    ; the constant is stored in al
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di contains x
    lea     bx, V
    add     [bx][di], al                                    ; Vx = Vx + kk
    jmp     execute_cycle_end

opcode_8xyu::
    mov     ax, [opcode]
    and     ax, 0000Fh
    shl     ax, 1
    lea     si, opcode_8xyu_jmptable
    add     si, ax
    jmp     word ptr [si]

opcode_8xy0::                                               ; opcode 8XY0h: Set Vx to the value of Vy (Vx = Vy)
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si contains y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di contains x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl contains Vy
    mov     [bx][di], dl                                    ; Vx = Vy
    jmp     execute_cycle_end
    
opcode_8xy1::                                               ; opcode 8XY1h: Set Vx to Vx OR Vy (Vx = Vx OR Vy) - logic OR
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl = Vy
    or      [bx][di], dl                                    ; Vx = Vx OR Vy
    jmp     execute_cycle_end
    
opcode_8xy2::                                               ; opcode 8XY2h: Set Vx to Vx AND Vy (Vx = Vx AND Vy) - logic AND
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl = Vy
    and     [bx][di], dl                                    ; Vx = Vx AND Vy
    jmp     execute_cycle_end
    
opcode_8xy3::                                               ; opcode 8XY3h: Set Vx to Vx XOR Vy (Vx = Vx XOR Vy) - logic XOR
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl = Vy
    xor     [bx][di], dl                                    ; Vx = Vx XOR Vy
    jmp     execute_cycle_end
    
opcode_8xy4::                                               ; opcode 8XY4h: Add Vy to Vx (Vx = Vx + Vy). VF = 1 if there is a carry.
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl = Vy
    add     [bx][di], dl                                    ; Vx = Vx + Vy
    setc    byte ptr [bx + 0Fh]                             ; VF = 1 if there is a carry
    jmp     execute_cycle_end
    
opcode_8xy5::                                               ; opcode 8XY5h: Subtract Vy from Vx (Vx = Vx - Vy). VF = 0 if there was borrow.
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][si]                                    ; dl = Vy
    sub     [bx][di], dl                                    ; Vx = Vx - Vy
    setnc   byte ptr [bx + 0Fh]                             ; VF = 0 if there was a borrow (contrary to borrow logic in x86)
    jmp     execute_cycle_end
    
opcode_8xy6::                                               ; opcode 8XY6h: Shift the bits right in Vx by 1. VF = LSb(Vx) before the shifting
    mov     ax, [opcode]
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    shr     byte ptr[bx][di], 1                             ; Vx = Vx >> 1
    setc    byte ptr [bx + 0Fh]                             ; VF = LSb(Vx) before the shifting (the 'fallen' bit)
    jmp     execute_cycle_end
    
opcode_8xy7::                                               ; opcode 8XY7h: Set Vx to Vy - Vx (Vx = Vy - Vx). VF = 0 if there was borrow
    mov     ax, [opcode]
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     dl, [bx][di]                                    ; dl = Vx
    mov     dh, [bx][si]                                    ; dh = Vy
    sub     dh, dl                                          ; dh = Vy - Vx
    setnc   byte ptr [bx + 0Fh]                             ; VF = 0 if there was borrow
    mov     [bx][di], dh                                    ; Vx = Vy - Vx
    jmp     execute_cycle_end
    
opcode_8xyE::                                               ; opcode 8XYEh: Shift the bits left in Vx by 1. VF = MSb(Vx), before the shifting
    mov     ax, [opcode]
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    shl     byte ptr [bx][di], 1                            ; Vx = Vx << 1
    setc    byte ptr [bx + 0Fh]                             ; VF = MSb(Vx), before the shifting (the 'fallen' bit)
    jmp     execute_cycle_end

opcode_9xy0::                                               ; opcode 9XY0h: Skip next instruction if Vx != Vy
    mov     ax, [opcode]
    mov     di, 00F0h
    and     di, ax
    shr     di, 4                                           ; di = y
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    mov     dl, [bx][di]                                    ; dl = Vy
    cmp     [bx][si], dl
    je      execute_cycle_end                               ; if Vx == Vy, increase PC by two
    add     word ptr [PC], 2                                ; if Vx != Vy, increase PC by two more (total 4)
    jmp     execute_cycle_end

opcode_Annn::                                               ; opcode ANNNh: Set I to address NNNh
    mov     ax, [opcode]
    and     ax, 0FFFh                                       ; get the address
    mov     [I], ax                                         ; I = NNNh
    jmp     execute_cycle_end
    
opcode_Bnnn::                                               ; opcode BNNNh: Jump to address (NNNh + V0)
    mov     ax, [opcode]
    and     ax, 0FFFh                                       ; get the address
    xor     dx, dx
    mov     dl, [V]                                         ; get V0
    add     ax, dx
    mov     [PC], ax                                        ; PC = V0 + NNNh
    jmp     execute_cycle_end

opcode_Cxkk::                                               ; opcode CXKKh: Set Vx to the AND of kk with random number
    call    GetRandom                                       ; generate random number
    mov     ax, [opcode]                                    ; the constant kk is stored in al
    mov     si, 00F0h
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    and     al, [rand]
    mov     [bx][di], al                                    ; Vx = kk AND rand
    jmp     execute_cycle_end

opcode_Dxyn::                                               ; opcode DXYNh: Draw sprite on coordinates (Vx, Vy), with width of 8 pixels and height of N pixels
                                                            ; the bytes for the sprite are retrieved from address stored in I (total N bytes). New sprite is drawn by
                                                            ; xor-ing with previous screen content. If some pixel turns off (changes from 1 to 0), VF = 1
    mov     ax, [opcode]
    lea     bx, V
    mov     si, 00F0h
    and     si, ax
    shr     si, 4                                           ; si = y
    mov     dh, [bx][si]                                    ; dh = Vy
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    mov     dl, [bx][si]                                    ; dl = Vx
    
    mov     byte ptr [bx + 0Fh], 0                          ; VF = 0
    
    mov     ch, al
    and     ch, 0Fh                                         ; ch contains the height of the sprite, in pixels (N, the number of rows in the sprite)
    
    lea     si, mem
    add     si, [I]                                         ; si contains the address for retrieving a row from the sprite
    
    xor     cl, cl                                          ; cl stores the current column (the current pixel) of the row
    
    mov     al, dh
    mov     bl, display_width
    mul     bl
    mov     di, ax
    movzx   ax, dl
    add     di, ax                                          ; di stores the offset from the top-left corner of the screen, coordinates (0, 0)
    lea     bx, display
    
opcode_Dxyn_row:
    mov     al, [si]                                        ; al contains the 8 pixels of the current row of the sprite
opcode_Dxyn_pixel:
    mov     dl, 80h
    shr     dl, cl
    and     dl, al                                          ; check if the new pixel on column cl is active (on)
    jz      opcode_Dxyn_next_pixel                          ; if no, go to the next pixel (xor-ing with 0 doesn't change the original bit)
                                                            ; if yes, check if the same pixel in the previous screen state was on and set VF if this is true (collision detected)
    cmp     byte ptr [bx][di], 1                            ; check if the pixel (x + cl, y + ch - N) is on
    jne     opcode_Dxyn_no_collision
    
    lea     bx, V                                           ; set VF to 1, collision detected
    mov     byte ptr [bx + 0Fh], 1
    lea     bx, display
opcode_Dxyn_no_collision:
    xor     byte ptr [bx][di], 1                            ; invert the pixel
    
opcode_Dxyn_next_pixel:
    inc     cl
    cmp     cl, 8                                           ; check if all pixels from the row were processed
    je      opcode_Dxyn_next_row                            ; if yes, go to the next row of the sprite
    inc     di
    jmp     opcode_Dxyn_pixel
    
opcode_Dxyn_next_row:
    dec     ch
    cmp     ch, 0
    jz      opcode_Dxyn_end
    add     di, display_width - 7                           ; set di for the next row of the interpreter's screen
    cmp     di, display_size                                ; if di is out of bounds the sprite is cut-off
    jge     opcode_Dxyn_end
    inc     si
    xor     cl, cl
    jmp     opcode_Dxyn_row
    
opcode_Dxyn_end:
    mov     byte ptr [disp_flag], 1                         ; set the disp_flag, to update the computer screen
    jmp     execute_cycle_end

opcode_Exuu::
    mov     ax, [opcode]
    and     ax, 0000Fh
    cmp     ax, 0000Eh
    je      opcode_Ex9E
    cmp     ax, 00001h
    je      opcode_ExA1
    jmp     opcode_invalid              

opcode_Ex9E:                                                ; opcode EX9Eh: skip next instruction if the key stored in Vx is pressed
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    mov     al, [bx][si]                                    ; al = Vx
    lea     bx, keys
    movzx   si, al
    
    cmp     byte ptr [bx][si], 1                            ; if some key is pressed, check if it is the one stored in Vx
    jne     execute_cycle_end                               ; if it is not pressed, increment PC only by two
    add     word ptr [PC], 2                                ; if it is pressed, increment PC by two more (total 4)
    call    ResetKeyState                                   ; reset the state of the keys
    jmp     execute_cycle_end
    
opcode_ExA1:                                                ; opcode EXA1h: skip next instruction if the key stored in Vx is NOT pressed
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    mov     al, [bx][si]                                    ; al = Vx
    lea     bx, keys
    movzx   si, al
    
    cmp     byte ptr [bx][si], 1                            ; if some key is pressed, check if it is the one stored in Vx
    je      opcode_ExA1_btn                                 ; if yes, increment PC only by two
opcode_ExA1_pc_inc4:
    add     word ptr [PC], 2                                ; if not, increment PC by two more (total 4)
    jmp     execute_cycle_end
opcode_ExA1_btn:
    call    ResetKeyState                                   ; reset the state of the keys
    jmp     execute_cycle_end
    
opcode_Fxuu::
    mov     ax, [opcode]
    and     ax, 0000Fh
    shl     ax, 1
    lea     si, opcode_Fxuu_jmptable
    add     si, ax
    jmp     word ptr [si]
   
opcode_Fx07::                                               ; opcode FX07h: store the delay timer value in Vx (Vx = DT)
    mov     ax, [opcode]
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     al, [delay_t]                                   ; al = DT
    mov     [bx][di], al                                    ; DT = Vx
    jmp     execute_cycle_end

opcode_Fx0A::                                               ; opcode Fx0Ah: Block the execution until any key is pressed. The pressed button is stored in Vx
    mov     cx, keypad_size
    lea     bx, keys
    xor     si, si                                          ; si will store the number of the key
    
opcode_Fx0A_check:
    cmp     byte ptr [bx][si], 1                            ; check which key was pressed
    jne     opcode_Fx0A_check_next
    
    mov     ax, [opcode]                                    ; if key was pressed, store its number in Vx and increment PC
    mov     di, 0F00h
    and     di, ax
    shr     di, 8                                           ; di = x
    lea     bx, V
    mov     ax, si                                          ; to store byte in Vx
    mov     [bx][di], al
    call    ResetKeyState                                   ; reset the state of the keys
    jmp     execute_cycle_end
    
opcode_Fx0A_check_next:
    inc     si
    loop    opcode_Fx0A_check
    
    sub     word ptr [PC], 2                                ; execute the same instruction again
    jmp     execute_cycle_end
  
opcode_Fxu5::
    mov     ax, [opcode]
    and     ax, 000F0h
    cmp     ax, 00010h
    je      opcode_Fx15
    cmp     ax, 00050h
    je      opcode_Fx55
    cmp     ax, 00060h
    je      opcode_Fx65
    jmp     opcode_invalid

opcode_Fx15:                                                ; opcode FX15h: load the delay timer with the value of Vx (DT = Vx)
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    mov     al, [bx][si]                                    ; al = Vx
    mov     [delay_t], al                                   ; DT = Vx
    jmp     execute_cycle_end

opcode_Fx55:                                                ; opcode FX55h: store the registers V0 to Vx (inclusive) in memory, on starting address stored in I
    mov     cx, [opcode]
    and     cx, 0F00h
    shr     cx, 8                                           ; cx = x
    inc     cx                                              ; x+1 registers need to be stored
    mov     ax, ds
    mov     es, ax
    mov     ax, cx
    lea     si, V
    lea     di, mem
    add     di, [I]
    rep     movsb                                           ; store the V registers in memory
    add     [I], ax                                         ; should we change the value of I? (increment it by x+1, or don't change it at all?)
    jmp     execute_cycle_end
    
opcode_Fx65:                                                ; opcode FX65h: load the registers V0 to Vx from memory, with starting address stored in I
    mov     cx, [opcode]
    and     cx, 0F00h
    shr     cx, 8                                           ; cx = x
    inc     cx                                              ; x+1 registers need to be loaded
    mov     ax, ds
    mov     es, ax
    mov     ax, cx
    lea     di, V
    lea     si, mem
    add     si, [I]
    rep     movsb                                           ; load Vx with the values from memory
    add     [I], ax                                         ; should we change the value of I? (increment it by x+1, or don't change it at all?)
    jmp     execute_cycle_end

opcode_Fx18::                                               ; opcode FX18h: load the sound timer with value of Vx (ST = Vx)
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    mov     al, [bx][si]                                    ; al = Vx
    mov     [sound_t], al                                   ; ST = Vx
    mov     byte ptr [beep_flag], 1                         ; flag the start of a beep
    jmp     execute_cycle_end

opcode_Fx1E::                                               ; opcode FX1Eh: add Vx to I (I = I + Vx)
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    movzx   ax, byte ptr[bx][si]                            ; ax = Vx (al = Vx, ah = zeros)
    add     [I], ax                                         ; I = I + Vx
    jmp     execute_cycle_end

opcode_Fx29::                                               ; opcode FX29h: load I with the starting address of the sprite for the hexadecimal digit stored in Vx
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    movzx   ax, byte ptr [bx][si]                           ; ax = Vx (al = Vx, ah = zeros)
    mov     dl, 5                                           ; the sprites for the hexadecimal digits (from fontset) are 5 bytes long
    mul     dl                                              ; ax contains the starting address for the corresponding sprite
    mov     [I], ax
    jmp     execute_cycle_end
    
opcode_Fx33::                                               ; opcode FX33h: store the BCD format, for the number stored in Vx, in memory, with addresses: I (hundreds), I + 1 (tens), I + 2 (ones)
    mov     ax, [opcode]
    mov     si, 0F00h
    and     si, ax
    shr     si, 8                                           ; si = x
    lea     bx, V
    movzx   ax, byte ptr [bx][si]                           ; ax = Vx (al = Vx, ah = zeros)
    lea     bx, mem
    mov     si, [I]
    mov     cl, 100
    div     cl                                              ; the number of hundreds are the quotient from the division with 100, the remainder are the remaining two digits
    mov     [bx][si], al                                    ; store the number of hundreds on address stored in I
    movzx   ax, ah
    mov     cl, 10
    div     cl                                              ; the number of tens is the quotient from division with 10, the remainder is the number of ones
    mov     [bx][si + 1], al                                ; store the number of tens on address I + 1
    mov     [bx][si + 2], ah                                ; store the number of ones on address I + 1
    jmp     execute_cycle_end
    
opcode_invalid::                                            ; invalid opcode
    nop
execute_cycle_end:
    pop     es
    popa
    ret
ExecuteCycle        endp

;-------------------------------------------------
; Procedure for setting the state of the keys 
; on the keypad
; 1. Reset the state of all keys to 0 (not pressed)
; 2. Check if key is pressed
; 3. If yes, find which key and update its state
;-------------------------------------------------
KeyState            proc
    pusha
    push    es
    
    mov     ah, 1h                                          ; Check if any key is pressed
    int     16h                                             ; ZF = 1 if no key is pressed
    jz      key_state_end
    
    mov     ah, 0h                                          ; get the ASCII value for the pressed key
    int     16h                                             ; al contains the ASCII value
                                                            ; check which key was pressed and update the state of the interpreter
    cmp     al, 1Bh
    je      key_esc                                         ; ESC, quit the interpreter
    
    cmp     al, '`'                                         ; bactick, reset the interpreter
    je      key_backq
    cmp     al, '['                                         ; [ - lower the number of instruction executed per frame
    je      key_leftsqbracket
    cmp     al, ']'                                         ; ] - increase the number of instructions executed per frame
    je      key_rightsqbracket
    
    call    ResetKeyState
    
    cmp     al, '1'
    je      key1
    cmp     al, '2'
    je      key2
    cmp     al, '3'
    je      key3
    cmp     al, '4'
    je      keyC
    cmp     al, 'q'
    je      key4
    cmp     al, 'w'
    je      key5
    cmp     al, 'e'
    je      key6
    cmp     al, 'r'
    je      keyD
    cmp     al, 'a'
    je      key7
    cmp     al, 's'
    je      key8
    cmp     al, 'd'
    je      key9
    cmp     al, 'f'
    je      keyE
    cmp     al, 'z'
    je      keyA
    cmp     al, 'x'
    je      key0
    cmp     al, 'c'
    je      keyB
    cmp     al, 'v'
    je      keyF
    jmp     key_state_end                                   ; key is not relevant for the interpreter
    
key_esc:
    mov     byte ptr [running], 0
    jmp     key_state_end
key_backq:
    call    Reset
    jmp     key_state_end
key_leftsqbracket:
    cmp     byte ptr [inst_pf], 1
    je      key_state_end
    dec     byte ptr [inst_pf]
    jmp     key_state_end
key_rightsqbracket:
    cmp     byte ptr [inst_pf], 30
    je      key_state_end
    inc     byte ptr [inst_pf]
    jmp     key_state_end
key0:
    mov     di, 0
    jmp     update_key_state
key1:
    mov     di, 1
    jmp     update_key_state
key2:
    mov     di, 2
    jmp     update_key_state
key3:
    mov     di, 3
    jmp     update_key_state
key4:
    mov     di, 4
    jmp     update_key_state
key5:
    mov     di, 5
    jmp     update_key_state
key6:
    mov     di, 6
    jmp     update_key_state
key7:
    mov     di, 7
    jmp     update_key_state
key8:
    mov     di, 8
    jmp     update_key_state    
key9:
    mov     di, 9
    jmp     update_key_state    
keyA:
    mov     di, 0Ah
    jmp     update_key_state
keyB:
    mov     di, 0Bh
    jmp     update_key_state
keyC:
    mov     di, 0Ch
    jmp     update_key_state
keyD:
    mov     di, 0Dh
    jmp     update_key_state
keyE:
    mov     di, 0Eh
    jmp     update_key_state
keyF:
    mov     di, 0Fh
    
update_key_state:
    lea     bx, keys
    mov     byte ptr [bx][di], 1                            ; update the state of the corresponding hex keypad key
key_state_end:
    pop     es
    popa
    ret
KeyState            endp

;-------------------------------------------------
; Procedure which resets the state of the keys
; of the hex keypad
;-------------------------------------------------
ResetKeyState       proc
    push    ax
    push    cx
    push    es
    
    mov     ax, ds
    mov     es, ax
    lea     di, keys
    mov     cx, keypad_size
    xor     al, al
    rep     stosb                                           ; Set the state of the keys to 0
    
    pop     es
    pop     cx
    pop     ax
    ret
ResetKeyState       endp

;-------------------------------------------------
; Procedure for initialization the pseudo-random
; number generator
;-------------------------------------------------
InitRandom      proc
    push    ax
    push    cx
    push    dx
    
    xor     ah, ah
    int     1Ah                                             ; cx:dx contains the number of ticks of the system timer, since midnight
    mov     word ptr [shift_reg], dx                        ; initialize the shift register
    mov     word ptr [shift_reg + 2], cx
    
    pop     dx
    pop     cx
    pop     ax
    ret
InitRandom      endp

;-------------------------------------------------
; Procedure for generating pseudo-random number
; using 32-bit LFSR (Linear-feedback shift register)
; For maximum period of the generated sequence
; xor bits on position 32, 30, 26 and 25
; of the 32 bit shift register (LSb is bit 1)
; The generated random number has 1 byte length
; and it is stored in rand
;-------------------------------------------------
GetRandom       proc
    push    ax
    push    bx

    mov     al, [shift_reg + 3]
    
    mov     bl, al
    and     bl, 20h
    shr     bl, 5                                           ; bl = bit 30 of shift_reg
    
    mov     bh, al
    and     bh, 80h
    shr     bh, 7                                           ; bh = bit 32 of shift_reg
    
    mov     ah, al
    and     ah, 2h
    shr     ah, 1                                           ; ah = bit 26 of shift_reg
    
    and     al, 1h                                          ; al = bit 25 of shift_reg
    
    xor     al, ah
    xor     al, bl
    xor     al, bh                                          ; al contains the new bit which should be shifted into shift_reg (the new bit 1)
    
    cmp     al, 0
    jz      shift
    stc                                                     ; if the new bit is not 0, set the carry flag (we will shift through the carry flag)
shift:                                                      ; shift left the shift register by one, via carry flag
    rcl     byte ptr [shift_reg], 1
    rcl     byte ptr [shift_reg + 1], 1
    rcl     byte ptr [shift_reg + 2], 1
    rcl     byte ptr [shift_reg + 3], 1
    
    mov     al, [shift_reg + 1]
    xor     al, [shift_reg + 2]
    mov     [rand], al                                      ; rand stores the random number
    
    pop     bx
    pop     ax
    ret
GetRandom       endp

;-------------------------------------------------
; Procedure for initializing the video mode
; 320x200, 256 colors
;-------------------------------------------------
InitVideo           proc
    push    ax
    
    mov     ax, 0F00h
    int     10h                                             ; get the old video mode
    mov     [video_mode], al                                ; store it
    
    mov     ax, 0013h                                       ; (al) = 13h for 256 colors, 320x200
    int     10h                                             ; set the new video mode
    
    pop     ax
    ret
InitVideo           endp

;-------------------------------------------------
; Procedure for restoring the old video mode
;-------------------------------------------------
RestoreVideo        proc
    push    ax
    
    movzx   ax, byte ptr [video_mode]
    int     10h                                             ; set the old video mode
    
    pop     ax
    ret
RestoreVideo        endp


;-------------------------------------------------
; Procedure for initializing the system timer
; to count with frequency 60 Hz,
; and setting the frequency of channel 2 counter
; to beep_f which will be used for generating beep
; sound on the speaker.
; The system timer will be used for updating the
; delay timer and sound timer.
; The value that needs to be stored in the system
; timer is 1193182 / 60 = 19 886
; Timer 0 has address 40h, timer 2 has address 42h,
; while the control register has address 43h.
; This procedure also changes the interrupt vector
; 1Ch with the address of the function TimersInt
;-------------------------------------------------
TimersInit      proc
    pusha
    push    es
    
    cli                                                     ; disable the interrupts from the peripheral devices during intialization
    
    mov     dx, 43h
    mov     al, 34h                                         ; counter 0 - first writing the LSB, then MSB, mode 2(rate generator), binary counting
    out     dx, al
    
    mov     dx, 40h
    mov     ax, 19886                                       ; setting the frequnecy of counter 0 to 60 Hz
    out     dx, al
    mov     al, ah
    out     dx, al
    
    mov     dx, 43h
    mov     al, 0B6h
    out     dx, al                                          ; counter 2 - first writing LSB, then MSB, mode 3(square wave generator), binary counting
    
    mov     dx, 42h
    mov     ax, [beep_f]                                    ; setting the frequency of counter 2 to beep_f
    out     dx, al
    mov     al, ah
    out     dx, al
    
    mov     ax, 351Ch                                       ; store the old interrupt vector for interrupt 1Ch
    int     21h
    mov     [int1c_bx], bx
    mov     ax, es
    mov     [int1c_es], ax
    
    push    ds                                              ; setting the new interrupt vector for interrupt 1Ch
    mov     ax, cs
    mov     ds, ax
    lea     dx, TimersInt
    mov     ax, 251Ch
    int     21h
    pop     ds
    
    sti                                                     ; enable the interrupts from the peripheral devices
    
    pop     es
    popa
    ret
TimersInit      endp

;-------------------------------------------------
; Service routine for interrupt 1Ch
; Decrements the delay and sound timer if they are 
; greater than zero
; Also starts and ends the beep sound
;-------------------------------------------------
TimersInt       proc
    push    ax
    push    dx
    push    ds
    mov     ax, data_seg
    mov     ds, ax
    
    cmp     byte ptr [delay_t], 0
    je      sound_timer_check
    dec     byte ptr [delay_t]
sound_timer_check:
    cmp     byte ptr [beep_flag], 1                         ; check if beep_flag is set
    jne     update_sound_timer
    mov     byte ptr [beep_flag], 0                         ; reset the beep_flag
beep_on:                                                    ; start the beep sound
    mov     dx, 61h
    in      al, dx                                          ; read port on address 61h
    or      al, 03h                                         ; set bits 0 and 1, to forward the square wave signals from counter 2 to the speaker
    out     dx, al                                          ; write the port to start the beep
    
update_sound_timer:
    cmp     byte ptr [sound_t], 0
    je      timers_int_end
    dec     byte ptr [sound_t]
    cmp     byte ptr [sound_t], 0
    jne     timers_int_end                                  ; if sound_t is 0 after decrementing, stop the beep sound
beep_off:
    mov     dx, 61h
    in      al, dx                                          ; read port on address 61h
    and     al, 0FCh                                        ; clear bits 0 and 1, to block the square wave signals from counter 2 to the speaker
    out     dx, al                                          ; write the port to end the beep
timers_int_end:
    pop     ds
    pop     dx
    pop     ax
    iret
TimersInt       endp

;-------------------------------------------------
; Procedure for restoring the old state of the
; system counter and the old interrupt vector
; for interrupt 1Ch
;-------------------------------------------------
TimersRestore   proc
    pusha
    
    cli
    
    push    ds                                              ; restore the old interrupt vector for interrupt 1Ch
    mov     dx, [int1c_bx]
    mov     ax, [int1c_es]
    mov     ds, ax
    mov     ax, 251Ch
    int     21h
    pop     ds
    
    mov     dx, 43h                                         ; restore the old state (configuration) of the system timer
    mov     al, 34h
    out     dx, al
    
    mov     dx, 40h
    mov     al, 0FFh
    out     dx, al
    out     dx, al
    
    mov     dx, 61h
    in      al, dx                                          ; read port on address 61h
    and     al, 0FCh                                        ; clear bits 0 and 1, to block the square wave signals from counter 2 to the speaker
    out     dx, al                                          ; write the port to end the beep sound if it was playing
    
    mov     dx, 42h
    mov     al, 0
    out     dx, al
    
    sti
    
    popa
    ret
TimersRestore   endp

;-------------------------------------------------
; Procedure for printing string on stdout
; Input parameters:
;  - dx - contains the starting address of the string
;-------------------------------------------------
PrintString     proc
    push    ax
    
    mov     ah, 09h
    int     21h
    
    pop     ax
    ret
PrintString     endp

;-------------------------------------------------
; Procedure for inputing string (the file name)
; Returns the entered string in fname
;-------------------------------------------------
GetFileName     proc
    pusha
    push    es
    
    lea     dx, msg_fname
    call    PrintString
    
    mov     ah, 0Ah
    lea     dx, in_buffer
    int     21h
    
    lea     si, in_buffer + 1
    movzx   cx, byte ptr [si]                               ; get the number of entered characters, excluding CR
    
    mov     ax, ds
    mov     es, ax
    
    lea     si, in_buffer + 2
    lea     di, fname
    rep     movsb                                           ; copy the entered string in fname
    
    pop     es
    popa
    ret
GetFileName     endp

;-------------------------------------------------
; Procedure for opening file
; Input parameters:
; - ds:dx - the address of the filename
; - al:
;     - 00 - read mode
;     - 01 - write mode
;     - 10 - read and write mode
; Return values:
; - ax - the file handle
; - carry flag - set if error occured, while ax
;   contains the error code
;-------------------------------------------------
OpenFile            proc
    mov     ah, 3Dh
    int     21h
    ret
OpenFile            endp

;-------------------------------------------------
; Procedure for closing file
; Input parameters:
; - bx - file handle
;-------------------------------------------------
CloseFile           proc
    mov     ah, 3Eh
    int     21h
    ret
CloseFile           endp

;-------------------------------------------------
; Procedure for obtaining the size of a file
; Vlezni argumenti:
; - bx - file handle
; Return values:
; - ax - the size of the file in bytes
;   (the size of the file should be smaller or equal
;   to 4016 bytes, so dx is ignored)
;-------------------------------------------------
FileSize            proc
    push    cx
    push    dx
    push    di
    
    mov     al, 02h
    mov     cx, 0
    mov     dx, 0
    mov     ah, 42h
    int     21h                                             ; seek to the end of the file
    
    mov     di, ax                                          ; ax contains the size of the file, in bytes
    
    mov     al, 0
    mov     cx, 0
    mov     dx, 0
    mov     ah, 42h
    int     21h                                             ; seek to the beginning of the file
    
    mov     ax, di
    
    pop     di
    pop     dx
    pop     cx
    ret
FileSize            endp

code_seg            ends

end                 start