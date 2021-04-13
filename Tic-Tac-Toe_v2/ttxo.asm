; space open 10,426 bytes

a_punch			=	$01
b_punch			=	$02
select_punch	=	$04
start_punch		=	$08
up_punch		=	$10
down_punch		=	$20
left_punch		=	$40
right_punch		=	$80
sprite			=	$200
sprite1			=	$204
sprite2			=	$208
sprite3			=	$20c
sprite4			=	$210
sprite5			=	$214
sprite6			=	$218
sprite7			=	$21c
sprite8			=	$220
sprite9			=	$224
sprite10		=	$228
init			=	$898a
play			=	$8977

.segment "ZEROPAGE"

control_pad:  .res 1
control_old:  .res 1
control_pad2: .res 1
control_old2: .res 1
game_start:   .res 1
whose_turn:   .res 1
stopleft:     .res 1
stop:         .res 1
stopright:    .res 1
sleft:        .res 1
smiddle:      .res 1
sright:       .res 1
sbottomleft:  .res 1
sbottom:      .res 1
sbottomright: .res 1
xtopleft:     .res 1
xtop:         .res 1
xtopright:    .res 1
xleft:        .res 1
xmiddle:      .res 1
xright:       .res 1
xbottomleft:  .res 1
xbottom:      .res 1
xbottomright: .res 1
otopleft:     .res 1
otop:         .res 1
otopright:    .res 1
oleft:        .res 1
omiddle:      .res 1
oright:       .res 1
obottomleft:  .res 1
obottom:      .res 1
obottomright: .res 1
atopleft:     .res 1
atop:         .res 1
atopright:    .res 1
aleft:        .res 1
amiddle:      .res 1
aright:       .res 1
abottomleft:  .res 1
abottom:      .res 1
abottomright: .res 1
stalecount:   .res 1
extra_turn:   .res 1
switch_to_x:  .res 1
clear_horiz:  .res 1
clear_vert:   .res 1
player_won:   .res 1
move_count:   .res 1
diag_win:     .res 1
horiz_win:    .res 1
vert_win:     .res 1
doing_item3:  .res 1
doing_item4:  .res 1
inc_win:      .res 1
no_control:   .res 1
comp_think:   .res 1
comp_think2:  .res 1
title_select: .res 1
instr_hold:   .res 1
instr_hold2:  .res 1
instr_switch: .res 1
instr_switch2:.res 1
cant_see_hidden: .res 1

.segment "CODE"

.incbin "ttxo.nsf"

reset:
    sei
	ldx #$00
	stx $4015
    ldx #$40
    stx $4017
    ldx #$ff
    txs
    inx
    stx $2000
    stx $2001

:	bit $2002
	bpl :-

    txa
clrmem:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne clrmem

:	bit $2002
	bpl :-

	ldx #$ff
	txs

	lda #$00					; turn off PPU
	sta $2000
	sta $2001
;	lda #$00					; Sprites
;	ldx #$00
;:	sta sprite, x
;	inx
;	bne :-
	lda #$3F					; Load palette
	ldx #$00
	sta $2006
	stx $2006
:	lda palette,x
	sta $2007
	inx
	cpx #$20
	bne :-

	lda #$01
	sta no_control
title_screen:
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
	ldy #$00					; load title screen
	ldx #$04
	lda #<title
	sta $10
	lda #>title
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
	jsr PPU_no_sprites
	jsr PPU_with_sprites

	ldx #$30
wait_for_tune:
:	bit $2002
	bpl :-
	dex
	bne wait_for_tune
	lda #$0						; Load song 1
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	
	lda #$01
	sta whose_turn
hold_title:				; YOU ARE HERE
	lda title_select
	cmp #$01
	bne keep_holding_title
	jmp instructions0
keep_holding_title:
	lda game_start
	cmp #$01
	bne hold_title

	lda #$01
	sta cant_see_hidden
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
	sta no_control
	ldy #$00					; load game screen
	ldx #$04
	lda #<board
	sta $10
	lda #>board
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
;	jsr PPU_no_sprites
	jsr PPU_with_sprites

	lda #$1						; Load song 2
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000

	lda #$00					; Load 0 in game_start so the sound
	sta game_start				; effects won't trigger through buttons
	ldx #$60					; Kill a bit of time before the sprites
wait_it_out:					; get loaded. Raise X above wait_it_out
:	bit $2002					; to waste more time. Use vblank to do
	bpl :-						; this. Decrement X and loop until X
	dex							; is at zero.
	bne wait_it_out

	lda #$01					; Set game_start back to 1 so the
	sta game_start				; sound effects will work w/ controls.

	ldx #$0						; Pull in bytes for sprites and their
:	lda the_sprites,x			; attributes which are stored in
	sta sprite,x				; the_sprites. Use X as an index
	inx							; to load and store each byte, which
    cpx #$2c					; get stored starting in $200, where
    bne :-						; 'sprite' is located at.

	lda #$2						; Load song 3
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000

reinit_variables:
	lda #$01
	sta stopleft
	sta stop
	sta stopright
	sta sright
	sta smiddle
	sta sleft
	sta sbottomleft
	sta sbottom
	sta sbottomright
	lda #$01
	sta atopleft
	sta atop
	sta atopright
	sta aright
	sta amiddle
	sta aleft
	sta abottomleft
	sta abottom
	sta abottomright
	lda #$00
	sta xtopleft
	sta xtop
	sta xtopright
	sta xleft
	sta xmiddle
	sta xright
	sta xbottomleft
	sta xbottom
	sta xbottomright
	lda #$00
	sta otopleft
	sta otop
	sta otopright
	sta oleft
	sta omiddle
	sta oright
	sta obottomleft
	sta obottom
	sta obottomright
	sta player_won
	sta move_count

	lda stalecount
	cmp #$03
	bne test_diag
	lda #$01
	sta extra_turn
	lda #$30
	sta sprite3+1
test_diag:
	lda diag_win
	cmp #$02
	bne test_horiz
	lda #$01
	sta switch_to_x
	lda #$30
	sta sprite5+1
test_horiz:
	lda horiz_win
	cmp #$02
	bne test_vert
	lda #$01
	sta clear_horiz
	lda #$30
	sta sprite7+1
test_vert:
	lda vert_win
	cmp #$02
	bne wait_turn
	lda #$01
	sta clear_vert
	lda #$30
	sta sprite9+1

wait_turn:
	lda move_count
	cmp #$09
	bne still_playing
	jsr PPU_no_sprites
	jmp end_computer
still_playing:
    lda whose_turn
    cmp #$00
    bne wait_turn

    jsr PPU_no_sprites

pattern1_first:
    lda otop
    beq pattern1_second
    lda otopright
    beq pattern1_second
    lda aleft
    beq pattern1_second
    lda amiddle
    beq pattern1_second
    lda aright
    beq pattern1_second
    lda abottomleft
    beq pattern1_second
    lda abottom
    beq pattern1_second
    lda abottomright
	beq pattern1_second
    lda stopleft
    beq pattern1_second
	jsr comp_topleft
	jmp computer_win
pattern1_second:
	lda atopleft
	beq pattern1_third
	lda atop
	beq pattern1_third
	lda aleft
	beq pattern1_third
	lda amiddle
	beq pattern1_third
	lda oright
	beq pattern1_third
	lda abottomleft
	beq pattern1_third
	lda abottom
	beq pattern1_third
	lda obottomright
	beq pattern1_third
	lda stopright
	beq pattern1_third
	jsr comp_topright
	jmp computer_win
pattern1_third:					; CHECK
	lda atopleft
	beq pattern1_fourth
	lda atop
	beq pattern1_fourth
	lda atopright
	beq pattern1_fourth
	lda aleft
	beq pattern1_fourth
	lda amiddle
	beq pattern1_fourth
	lda aright
	beq pattern1_fourth
	lda obottomleft
	beq pattern1_fourth
	lda obottom
	beq pattern1_fourth
	lda sbottomright
	beq pattern1_fourth
	jsr comp_bottomright
	jmp computer_win
pattern1_fourth:				; CHECK
	lda otopleft
	beq pattern2_first
	lda atop
	beq pattern2_first
	lda atopright
	beq pattern2_first
	lda oleft
	beq pattern2_first
	lda amiddle
	beq pattern2_first
	lda aright
	beq pattern2_first
	lda abottom
	beq pattern2_first
	lda abottomright
	beq pattern2_first
	lda sbottomleft
	beq pattern2_first
	jsr comp_bottomleft
	jmp computer_win
	
pattern2_first:					; PATTERN 2-1 LOOKS GOOD ROB!!!
	lda atop
	beq pattern2_second
	lda atopright
	beq pattern2_second
	lda oleft
	beq pattern2_second
	lda amiddle
	beq pattern2_second
	lda aright
	beq pattern2_second
	lda obottomleft
	beq pattern2_second
	lda abottom
	beq pattern2_second
	lda abottomright
	beq pattern2_second
	lda stopleft
	beq pattern2_second
	jsr comp_topleft
	jmp computer_win
pattern2_second:
	lda otopleft
	beq pattern2_third
	lda otop
	beq pattern2_third
	lda aleft
	beq pattern2_third
	lda amiddle
	beq pattern2_third
	lda aright
	beq pattern2_third
	lda abottomleft
	beq pattern2_third
	lda abottom
	beq pattern2_third
	lda abottomright
	beq pattern2_third
	lda stopright
	beq pattern2_third
	jsr comp_topright
	jmp computer_win
pattern2_third:
	lda atopleft
	beq pattern2_fourth
	lda atop
	beq pattern2_fourth
	lda otopright
	beq pattern2_fourth
	lda aleft
	beq pattern2_fourth
	lda amiddle
	beq pattern2_fourth
	lda oright
	beq pattern2_fourth
	lda abottomleft
	beq pattern2_fourth
	lda abottom
	beq pattern2_fourth
	lda sbottomright
	beq pattern2_fourth
	jsr comp_bottomright
	jmp computer_win
pattern2_fourth:
	lda atopleft
	beq pattern3_first
	lda atop
	beq pattern3_first
	lda atopright
	beq pattern3_first
	lda aleft
	beq pattern3_first
	lda amiddle
	beq pattern3_first
	lda aright
	beq pattern3_first
	lda obottom
	beq pattern3_first
	lda obottomright
	beq pattern3_first
	lda sbottomleft
	beq pattern3_first
	jsr comp_bottomleft
	jmp computer_win

pattern3_first:					; ALL OF PATTERN 3 LOOKS GOOD ROB!!!
	lda atop
	beq pattern3_second
	lda atopright
	beq pattern3_second
	lda aleft
	beq pattern3_second
	lda omiddle
	beq pattern3_second
	lda aright
	beq pattern3_second
	lda abottomleft
	beq pattern3_second
	lda abottom
	beq pattern3_second
	lda obottomright
	beq pattern3_second
	lda stopleft
	beq pattern3_second
	jsr comp_topleft
	jmp computer_win
pattern3_second:
	lda atopleft
	beq pattern3_third
	lda atop
	beq pattern3_third
	lda aleft
	beq pattern3_third
	lda omiddle
	beq pattern3_third
	lda aright
	beq pattern3_third
	lda obottomleft
	beq pattern3_third
	lda abottom
	beq pattern3_third
	lda abottomright
	beq pattern3_third
	lda stopright
	beq pattern3_third
	jsr comp_topright
	jmp computer_win
pattern3_third:
	lda otopleft
	beq pattern3_fourth
	lda atop
	beq pattern3_fourth
	lda atopright
	beq pattern3_fourth
	lda aleft
	beq pattern3_fourth
	lda omiddle
	beq pattern3_fourth
	lda aright
	beq pattern3_fourth
	lda abottomleft
	beq pattern3_fourth
	lda abottom
	beq pattern3_fourth
	lda sbottomright
	beq pattern3_fourth
	jsr comp_bottomright
	jmp computer_win
pattern3_fourth:
	lda atopleft
	beq pattern4_first
	lda atop
	beq pattern4_first
	lda otopright
	beq pattern4_first
	lda aleft
	beq pattern4_first
	lda omiddle
	beq pattern4_first
	lda aright
	beq pattern4_first
	lda abottom
	beq pattern4_first
	lda abottomright
	beq pattern4_first
	lda sbottomleft
	beq pattern4_first
	jsr comp_bottomleft
	jmp computer_win

pattern4_first:					; ALL OF PATTERN 4 LOOKS GOOD ROB!!!
	lda otopleft
	beq pattern4_second
	lda otopright
	beq pattern4_second
	lda aleft
	beq pattern4_second
	lda amiddle
	beq pattern4_second
	lda aright
	beq pattern4_second
	lda abottomleft
	beq pattern4_second
	lda abottom
	beq pattern4_second
	lda abottomright
	beq pattern4_second
	lda stop
	beq pattern4_second
	jsr comp_top
	jmp computer_win
pattern4_second:
	lda atopleft
	beq pattern4_third
	lda atop
	beq pattern4_third
	lda otopright
	beq pattern4_third
	lda aleft
	beq pattern4_third
	lda amiddle
	beq pattern4_third
	lda abottomleft
	beq pattern4_third
	lda abottom
	beq pattern4_third
	lda obottomright
	beq pattern4_third
	lda sright
	beq pattern4_third
	jsr comp_right
	jmp computer_win
pattern4_third:
	lda atopleft
	beq pattern4_fourth
	lda atop
	beq pattern4_fourth
	lda atopright
	beq pattern4_fourth
	lda aleft
	beq pattern4_fourth
	lda amiddle
	beq pattern4_fourth
	lda aright
	beq pattern4_fourth
	lda obottomleft
	beq pattern4_fourth
	lda obottomright
	beq pattern4_fourth
	lda sbottom
	beq pattern4_fourth
	jsr comp_bottom
	jmp computer_win
pattern4_fourth:
	lda otopleft
	beq pattern5_first
	lda atop
	beq pattern5_first
	lda atopright
	beq pattern5_first
	lda amiddle
	beq pattern5_first
	lda aright
	beq pattern5_first
	lda obottomleft
	beq pattern5_first
	lda abottom
	beq pattern5_first
	lda abottomright
	beq pattern5_first
	lda sleft
	beq pattern5_first
	jsr comp_left
	jmp computer_win

pattern5_first:					; ALL OF PATTERN 5 LOOKS GOOD ROB!!!
	lda atopleft
	beq pattern5_second
	lda atopright
	beq pattern5_second
	lda aleft
	beq pattern5_second
	lda omiddle
	beq pattern5_second
	lda aright
	beq pattern5_second
	lda abottomleft
	beq pattern5_second
	lda obottom
	beq pattern5_second
	lda abottomright
	beq pattern5_second
	lda stop
	beq pattern5_second
	jsr comp_top
	jmp computer_win
pattern5_second:
	lda atopleft
	beq pattern5_third
	lda atop
	beq pattern5_third
	lda atopright
	beq pattern5_third
	lda omiddle
	beq pattern5_third
	lda oleft
	beq pattern5_third
	lda abottomleft
	beq pattern5_third
	lda abottom
	beq pattern5_third
	lda abottomright
	beq pattern5_third
	lda sright
	beq pattern5_third
	jsr comp_right
	jmp computer_win
pattern5_third:
	lda atopleft
	beq pattern5_fourth
	lda atopright
	beq pattern5_fourth
	lda aleft
	beq pattern5_fourth
	lda omiddle
	beq pattern5_fourth
	lda otop
	beq pattern5_fourth
	lda aright
	beq pattern5_fourth
	lda abottomleft
	beq pattern5_fourth
	lda abottomright
	beq pattern5_fourth
	lda sbottom
	beq pattern5_fourth
	jsr comp_bottom
	jmp computer_win
pattern5_fourth:
	lda atopleft
	beq pattern_extra_first
	lda atop
	beq pattern_extra_first
	lda atopright
	beq pattern_extra_first
	lda omiddle
	beq pattern_extra_first
	lda oright
	beq pattern_extra_first
	lda abottomleft
	beq pattern_extra_first
	lda abottom
	beq pattern_extra_first
	lda abottomright
	beq pattern_extra_first
	lda sleft
	beq pattern_extra_first
	jsr comp_left
	jmp computer_win

pattern_extra_first:
	lda otopleft
	beq pattern_extra_second
	lda atop
	beq pattern_extra_second
	lda atopright
	beq pattern_extra_second
	lda aleft
	beq pattern_extra_second
	lda aright
	beq pattern_extra_second
	lda abottomleft
	beq pattern_extra_second
	lda abottom
	beq pattern_extra_second
	lda obottomright
	beq pattern_extra_second
	lda smiddle
	beq pattern_extra_second
	jsr comp_middle
	jmp computer_win
pattern_extra_second:
	lda atopleft
	beq pattern_extra_third
	lda atop
	beq pattern_extra_third
	lda otopright
	beq pattern_extra_third
	lda aleft
	beq pattern_extra_third
	lda aright
	beq pattern_extra_third
	lda obottomleft
	beq pattern_extra_third
	lda abottom
	beq pattern_extra_third
	lda abottomright
	beq pattern_extra_third
	lda smiddle
	beq pattern_extra_third
	jsr comp_middle
	jmp computer_win
pattern_extra_third:
	lda atopleft
	beq pattern_extra_fourth
	lda otop
	beq pattern_extra_fourth
	lda atopright
	beq pattern_extra_fourth
	lda aleft
	beq pattern_extra_fourth
	lda aright
	beq pattern_extra_fourth
	lda abottomleft
	beq pattern_extra_fourth
	lda obottom
	beq pattern_extra_fourth
	lda abottomright
	beq pattern_extra_fourth
	lda smiddle
	beq pattern_extra_fourth
	jsr comp_middle
	jmp computer_win
pattern_extra_fourth:
	lda atopleft
	beq pattern6_first
	lda atop
	beq pattern6_first
	lda atopright
	beq pattern6_first
	lda oleft
	beq pattern6_first
	lda oright
	beq pattern6_first
	lda abottomleft
	beq pattern6_first
	lda abottom
	beq pattern6_first
	lda abottomright
	beq pattern6_first
	lda smiddle
	beq pattern6_first
	jsr comp_middle
	jmp computer_win

pattern6_first:
    lda xtop
    beq pattern6_second
    lda xtopright
    beq pattern6_second
    lda aleft
    beq pattern6_second
    lda amiddle
    beq pattern6_second
    lda aright
    beq pattern6_second
    lda abottomleft
    beq pattern6_second
    lda abottom
    beq pattern6_second
    lda abottomright
    beq pattern6_second
    lda stopleft
    beq pattern6_second
	jsr comp_topleft
	jmp wait_turn
pattern6_second:				; CHECK
	lda atopleft
	beq pattern6_third
	lda atop
	beq pattern6_third
	lda aleft
	beq pattern6_third
	lda amiddle
	beq pattern6_third
	lda xright
	beq pattern6_third
	lda abottomleft
	beq pattern6_third
	lda abottom
	beq pattern6_third
	lda xbottomright
	beq pattern6_third
	lda stopright
	beq pattern6_third
	jsr comp_topright
	jmp wait_turn
pattern6_third:					; CHECK
	lda atopleft
	beq pattern6_fourth
	lda atop
	beq pattern6_fourth
	lda atopright
	beq pattern6_fourth
	lda aleft
	beq pattern6_fourth
	lda amiddle
	beq pattern6_fourth
	lda aright
	beq pattern6_fourth
	lda xbottomleft
	beq pattern6_fourth
	lda xbottom
	beq pattern6_fourth
	lda sbottomright
	beq pattern6_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern6_fourth:				; CHECK
	lda xtopleft
	beq pattern7_first
	lda atop
	beq pattern7_first
	lda atopright
	beq pattern7_first
	lda xleft
	beq pattern7_first
	lda amiddle
	beq pattern7_first
	lda aright
	beq pattern7_first
	lda abottom
	beq pattern7_first
	lda abottomright
	beq pattern7_first
	lda sbottomleft
	beq pattern7_first
	jsr comp_bottomleft
	jmp wait_turn
	
pattern7_first:					; PATTERN 2-1 LOOKS GOOD ROB!!!
	lda atop
	beq pattern7_second
	lda atopright
	beq pattern7_second
	lda xleft
	beq pattern7_second
	lda amiddle
	beq pattern7_second
	lda aright
	beq pattern7_second
	lda xbottomleft
	beq pattern7_second
	lda abottom
	beq pattern7_second
	lda abottomright
	beq pattern7_second
	lda stopleft
	beq pattern7_second
	jsr comp_topleft
	jmp wait_turn
pattern7_second:
	lda xtopleft
	beq pattern7_third
	lda xtop
	beq pattern7_third
	lda aleft
	beq pattern7_third
	lda amiddle
	beq pattern7_third
	lda aright
	beq pattern7_third
	lda abottomleft
	beq pattern7_third
	lda abottom
	beq pattern7_third
	lda abottomright
	beq pattern7_third
	lda stopright
	beq pattern7_third
	jsr comp_topright
	jmp wait_turn
pattern7_third:
	lda atopleft
	beq pattern7_fourth
	lda atop
	beq pattern7_fourth
	lda xtopright
	beq pattern7_fourth
	lda aleft
	beq pattern7_fourth
	lda amiddle
	beq pattern7_fourth
	lda xright
	beq pattern7_fourth
	lda abottomleft
	beq pattern7_fourth
	lda abottom
	beq pattern7_fourth
	lda sbottomright
	beq pattern7_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern7_fourth:
	lda atopleft
	beq pattern8_first
	lda atop
	beq pattern8_first
	lda atopright
	beq pattern8_first
	lda aleft
	beq pattern8_first
	lda amiddle
	beq pattern8_first
	lda aright
	beq pattern8_first
	lda xbottom
	beq pattern8_first
	lda xbottomright
	beq pattern8_first
	lda sbottomleft
	beq pattern8_first
	jsr comp_bottomleft
	jmp wait_turn

pattern8_first:					; ALL OF PATTERN 3 LOOKS GOOD ROB!!!
	lda atop
	beq pattern8_second
	lda atopright
	beq pattern8_second
	lda aleft
	beq pattern8_second
	lda xmiddle
	beq pattern8_second
	lda aright
	beq pattern8_second
	lda abottomleft
	beq pattern8_second
	lda abottom
	beq pattern8_second
	lda xbottomright
	beq pattern8_second
	lda stopleft
	beq pattern8_second
	jsr comp_topleft
	jmp wait_turn
pattern8_second:
	lda atopleft
	beq pattern8_third
	lda atop
	beq pattern8_third
	lda aleft
	beq pattern8_third
	lda xmiddle
	beq pattern8_third
	lda aright
	beq pattern8_third
	lda xbottomleft
	beq pattern8_third
	lda abottom
	beq pattern8_third
	lda abottomright
	beq pattern8_third
	lda stopright
	beq pattern8_third
	jsr comp_topright
	jmp wait_turn
pattern8_third:
	lda xtopleft
	beq pattern8_fourth
	lda atop
	beq pattern8_fourth
	lda atopright
	beq pattern8_fourth
	lda aleft
	beq pattern8_fourth
	lda xmiddle
	beq pattern8_fourth
	lda aright
	beq pattern8_fourth
	lda abottomleft
	beq pattern8_fourth
	lda abottom
	beq pattern8_fourth
	lda sbottomright
	beq pattern8_fourth
	jsr comp_bottomright
	jmp wait_turn	
pattern8_fourth:
	lda atopleft
	beq pattern9_first
	lda atop
	beq pattern9_first
	lda xtopright
	beq pattern9_first
	lda aleft
	beq pattern9_first
	lda xmiddle
	beq pattern9_first
	lda aright
	beq pattern9_first
	lda abottom
	beq pattern9_first
	lda abottomright
	beq pattern9_first
	lda sbottomleft
	beq pattern9_first
	jsr comp_bottomleft
	jmp wait_turn

pattern9_first:					; ALL OF PATTERN 4 LOOKS GOOD ROB!!!
	lda xtopleft
	cmp #$01
	bne pattern9_second
	lda xtopright
	cmp #$01
	bne pattern9_second
	lda aleft
	cmp #$01
	bne pattern9_second
	lda amiddle
	cmp #$01
	bne pattern9_second
	lda aright
	cmp #$01
	bne pattern9_second
	lda abottomleft
	cmp #$01
	bne pattern9_second
	lda abottom
	cmp #$01
	bne pattern9_second
	lda abottomright
	cmp #$01
	bne pattern9_second
	lda stop
	cmp #$01
	bne pattern9_second
	jsr comp_top
	jmp wait_turn
pattern9_second:
	lda atopleft
	cmp #$01
	bne pattern9_third
	lda atop
	cmp #$01
	bne pattern9_third
	lda xtopright
	cmp #$01
	bne pattern9_third
	lda aleft
	cmp #$01
	bne pattern9_third
	lda amiddle
	cmp #$01
	bne pattern9_third
	lda abottomleft
	cmp #$01
	bne pattern9_third
	lda abottom
	cmp #$01
	bne pattern9_third
	lda xbottomright
	cmp #$01
	bne pattern9_third
	lda sright
	cmp #$01
	bne pattern9_third
	jsr comp_right
	jmp wait_turn
pattern9_third:
	lda atopleft
	cmp #$01
	bne pattern9_fourth
	lda atop
	cmp #$01
	bne pattern9_fourth
	lda atopright
	cmp #$01
	bne pattern9_fourth
	lda aleft
	cmp #$01
	bne pattern9_fourth
	lda amiddle
	cmp #$01
	bne pattern9_fourth
	lda aright
	cmp #$01
	bne pattern9_fourth
	lda xbottomleft
	cmp #$01
	bne pattern9_fourth
	lda xbottomright
	cmp #$01
	bne pattern9_fourth
	lda sbottom
	cmp #$01
	bne pattern9_fourth
	jsr comp_bottom
	jmp wait_turn
pattern9_fourth:
	lda xtopleft
	cmp #$01
	bne pattern10_first
	lda atop
	cmp #$01
	bne pattern10_first
	lda atopright
	cmp #$01
	bne pattern10_first
	lda amiddle
	cmp #$01
	bne pattern10_first
	lda aright
	cmp #$01
	bne pattern10_first
	lda xbottomleft
	cmp #$01
	bne pattern10_first
	lda abottom
	cmp #$01
	bne pattern10_first
	lda abottomright
	cmp #$01
	bne pattern10_first
	lda sleft
	cmp #$01
	bne pattern10_first
	jsr comp_left
	jmp wait_turn

pattern10_first:					; ALL OF PATTERN 5 LOOKS GOOD ROB!!!
	lda atopleft
	cmp #$01
	bne pattern10_second
	lda atopright
	cmp #$01
	bne pattern10_second
	lda aleft
	cmp #$01
	bne pattern10_second
	lda xmiddle
	cmp #$01
	bne pattern10_second
	lda aright
	cmp #$01
	bne pattern10_second
	lda abottomleft
	cmp #$01
	bne pattern10_second
	lda xbottom
	cmp #$01
	bne pattern10_second
	lda abottomright
	cmp #$01
	bne pattern10_second
	lda stop
	cmp #$01
	bne pattern10_second
	jsr comp_top
	jmp wait_turn
pattern10_second:
	lda atopleft
	cmp #$01
	bne pattern10_third
	lda atop
	cmp #$01
	bne pattern10_third
	lda atopright
	cmp #$01
	bne pattern10_third
	lda xmiddle
	cmp #$01
	bne pattern10_third
	lda xleft
	cmp #$01
	bne pattern10_third
	lda abottomleft
	cmp #$01
	bne pattern10_third
	lda abottom
	cmp #$01
	bne pattern10_third
	lda abottomright
	cmp #$01
	bne pattern10_third
	lda sright
	cmp #$01
	bne pattern10_third
	jsr comp_right
	jmp wait_turn
pattern10_third:
	lda atopleft
	cmp #$01
	bne pattern10_fourth
	lda xtop
	cmp #$01
	bne pattern10_fourth
	lda atopright
	cmp #$01
	bne pattern10_fourth
	lda aleft
	cmp #$01
	bne pattern10_fourth
	lda xmiddle
	cmp #$01
	bne pattern10_fourth
	lda aright
	cmp #$01
	bne pattern10_fourth
	lda abottomleft
	cmp #$01
	bne pattern10_fourth
	lda abottomright
	cmp #$01
	bne pattern10_fourth
	lda sbottom
	cmp #$01
	bne pattern10_fourth
	jsr comp_bottom
	jmp wait_turn
pattern10_fourth:
	lda atopleft
	cmp #$01
	bne extra_pattern_again
	lda atop
	cmp #$01
	bne extra_pattern_again
	lda atopright
	cmp #$01
	bne extra_pattern_again
	lda xmiddle
	cmp #$01
	bne extra_pattern_again
	lda xright
	cmp #$01
	bne extra_pattern_again
	lda abottomleft
	cmp #$01
	bne extra_pattern_again
	lda abottom
	cmp #$01
	bne extra_pattern_again
	lda abottomright
	cmp #$01
	bne extra_pattern_again
	lda sleft
	cmp #$01
	bne extra_pattern_again
	jsr comp_left
	jmp wait_turn

extra_pattern_again:
	lda atopleft
	cmp #$01
	bne pattern11_first
	lda atop
	cmp #$01
	bne pattern11_first
	lda xtopright
	cmp #$01
	bne pattern11_first
	lda omiddle
	cmp #$01
	bne pattern11_first
	lda aright
	cmp #$01
	bne pattern11_first
	lda abottomleft
	cmp #$01
	bne pattern11_first
	lda xbottom
	cmp #$01
	bne pattern11_first
	lda sbottomright
	cmp #$01
	bne pattern11_first
	lda aleft
	cmp #$01
	bne pattern11_first
	jsr comp_bottomright
	jmp wait_turn

pattern11_first:
	lda stopleft
	cmp #$01
	bne pattern11_second
	lda xtopright
	cmp #$01
	bne pattern11_second
	lda sleft
	cmp #$01
	bne pattern11_second
	lda omiddle
	cmp #$01
	bne pattern11_second
	lda sright
	cmp #$01
	bne pattern11_second
	lda xbottomleft
	cmp #$01
	bne pattern11_second
	lda sbottom
	cmp #$01
	bne pattern11_second
	lda sbottomright
	cmp #$01
	bne pattern11_second
	lda stop
	cmp #$01
	bne pattern11_second
	jsr comp_top
	jmp wait_turn
pattern11_second:
	lda xtopleft
	cmp #$01
	bne pattern11_third
	lda stop
	cmp #$01
	bne pattern11_third
	lda stopright
	cmp #$01
	bne pattern11_third
	lda sleft
	cmp #$01
	bne pattern11_third
	lda omiddle
	cmp #$01
	bne pattern11_third
	lda sbottomleft
	cmp #$01
	bne pattern11_third
	lda sbottom
	cmp #$01
	bne pattern11_third
	lda xbottomright
	cmp #$01
	bne pattern11_third
	lda sright
	cmp #$01
	bne pattern11_third
	jsr comp_right
	jmp wait_turn
pattern11_third:
	lda stopleft
	cmp #$01
	bne pattern11_fourth
	lda stop
	cmp #$01
	bne pattern11_fourth
	lda xtopright
	cmp #$01
	bne pattern11_fourth
	lda sleft
	cmp #$01
	bne pattern11_fourth
	lda omiddle
	cmp #$01
	bne pattern11_fourth
	lda sright
	cmp #$01
	bne pattern11_fourth
	lda xbottomleft
	cmp #$01
	bne pattern11_fourth
	lda sbottomright
	cmp #$01
	bne pattern11_fourth
	lda sbottom
	cmp #$01
	bne pattern11_fourth
	jsr comp_bottom
	jmp wait_turn
pattern11_fourth:
	lda xtopleft
	cmp #$01
	bne pattern12_first
	lda stop
	cmp #$01
	bne pattern12_first
	lda stopright
	cmp #$01
	bne pattern12_first
	lda omiddle
	cmp #$01
	bne pattern12_first
	lda sright
	cmp #$01
	bne pattern12_first
	lda sbottomleft
	cmp #$01
	bne pattern12_first
	lda sbottom
	cmp #$01
	bne pattern12_first
	lda xbottomright
	cmp #$01
	bne pattern12_first
	lda sleft
	cmp #$01
	bne pattern12_first
	jsr comp_left
	jmp wait_turn

pattern12_first:
	lda xtop
	cmp #$01
	bne pattern12_second
	lda stopright
	cmp #$01
	bne pattern12_second
	lda omiddle
	cmp #$01
	bne pattern12_second
	lda xleft
	cmp #$01
	bne pattern12_second
	lda aright
	cmp #$01
	bne pattern12_second
	lda sbottomleft
	cmp #$01
	bne pattern12_second
	lda abottom
	cmp #$01
	bne pattern12_second
	lda abottomright
	cmp #$01
	bne pattern12_second
	lda stopleft
	cmp #$01
	bne pattern12_second
	jsr comp_topleft
	jmp wait_turn
pattern12_second:
	lda stopleft
	cmp #$01
	bne pattern12_third
	lda xtop
	cmp #$01
	bne pattern12_third
	lda aleft
	cmp #$01
	bne pattern12_third
	lda omiddle
	cmp #$01
	bne pattern12_third
	lda xright
	cmp #$01
	bne pattern12_third
	lda abottomleft
	cmp #$01
	bne pattern12_third
	lda abottom
	cmp #$01
	bne pattern12_third
	lda sbottomright
	cmp #$01
	bne pattern12_third
	lda stopright
	cmp #$01
	bne pattern12_third
	jsr comp_topright
	jmp wait_turn
pattern12_third:
	lda atopleft
	cmp #$01
	bne pattern12_fourth
	lda atop
	cmp #$01
	bne pattern12_fourth
	lda stopright
	cmp #$01
	bne pattern12_fourth
	lda aleft
	cmp #$01
	bne pattern12_fourth
	lda xright
	cmp #$01
	bne pattern12_fourth
	lda omiddle
	cmp #$01
	bne pattern12_fourth
	lda sbottomleft
	cmp #$01
	bne pattern12_fourth
	lda xbottom
	cmp #$01
	bne pattern12_fourth
	lda sbottomright
	cmp #$01
	bne pattern12_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern12_fourth:
	lda stopleft
	cmp #$01
	bne pattern13_first
	lda atop
	cmp #$01
	bne pattern13_first
	lda atopright
	cmp #$01
	bne pattern13_first
	lda xleft
	cmp #$01
	bne pattern13_first
	lda omiddle
	cmp #$01
	bne pattern13_first
	lda aright
	cmp #$01
	bne pattern13_first
	lda xbottom
	cmp #$01
	bne pattern13_first
	lda sbottomright
	cmp #$01
	bne pattern13_first
	lda sbottomleft
	cmp #$01
	bne pattern13_first
	jsr comp_bottomleft
	jmp wait_turn

pattern13_first:
	lda stop
	cmp #$01
	bne pattern13_second
	lda xtopright
	cmp #$01
	bne pattern13_second
	lda omiddle
	cmp #$01
	bne pattern13_second
	lda xleft
	cmp #$01
	bne pattern13_second
	lda aright
	cmp #$01
	bne pattern13_second
	lda sbottomleft
	cmp #$01
	bne pattern13_second
	lda abottom
	cmp #$01
	bne pattern13_second
	lda abottomright
	cmp #$01
	bne pattern13_second
	lda stopleft
	cmp #$01
	bne pattern13_second
	jsr comp_topleft
	jmp wait_turn
pattern13_second:
	lda stopleft
	cmp #$01
	bne pattern13_third
	lda xtop
	cmp #$01
	bne pattern13_third
	lda aleft
	cmp #$01
	bne pattern13_third
	lda omiddle
	cmp #$01
	bne pattern13_third
	lda sright
	cmp #$01
	bne pattern13_third
	lda abottomleft
	cmp #$01
	bne pattern13_third
	lda abottom
	cmp #$01
	bne pattern13_third
	lda xbottomright
	cmp #$01
	bne pattern13_third
	lda stopright
	cmp #$01
	bne pattern13_third
	jsr comp_topright
	jmp wait_turn
pattern13_third:
	lda atopleft
	cmp #$01
	bne pattern13_fourth
	lda atop
	cmp #$01
	bne pattern13_fourth
	lda stopright
	cmp #$01
	bne pattern13_fourth
	lda aleft
	cmp #$01
	bne pattern13_fourth
	lda xright
	cmp #$01
	bne pattern13_fourth
	lda omiddle
	cmp #$01
	bne pattern13_fourth
	lda xbottomleft
	cmp #$01
	bne pattern13_fourth
	lda sbottom
	cmp #$01
	bne pattern13_fourth
	lda sbottomright
	cmp #$01
	bne pattern13_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern13_fourth:
	lda xtopleft
	cmp #$01
	bne pattern14_first
	lda atop
	cmp #$01
	bne pattern14_first
	lda atopright
	cmp #$01
	bne pattern14_first
	lda sleft
	cmp #$01
	bne pattern14_first
	lda omiddle
	cmp #$01
	bne pattern14_first
	lda aright
	cmp #$01
	bne pattern14_first
	lda xbottom
	cmp #$01
	bne pattern14_first
	lda sbottomright
	cmp #$01
	bne pattern14_first
	lda sbottomleft
	cmp #$01
	bne pattern14_first
	jsr comp_bottomleft
	jmp wait_turn

pattern14_first:
	lda stop
	cmp #$01
	bne pattern14_second
	lda xtopright
	cmp #$01
	bne pattern14_second
	lda sleft
	cmp #$01
	bne pattern14_second
	lda omiddle
	cmp #$01
	bne pattern14_second
	lda aright
	cmp #$01
	bne pattern14_second
	lda xbottomleft
	cmp #$01
	bne pattern14_second
	lda abottom
	cmp #$01
	bne pattern14_second
	lda abottomright
	cmp #$01
	bne pattern14_second
	lda stopleft
	cmp #$01
	bne pattern14_second
	jsr comp_topleft
	jmp wait_turn
pattern14_second:
	lda xtopleft
	cmp #$01
	bne pattern14_third
	lda stop
	cmp #$01
	bne pattern14_third
	lda aleft
	cmp #$01
	bne pattern14_third
	lda omiddle
	cmp #$01
	bne pattern14_third
	lda sright
	cmp #$01
	bne pattern14_third
	lda abottomleft
	cmp #$01
	bne pattern14_third
	lda abottom
	cmp #$01
	bne pattern14_third
	lda xbottomright
	cmp #$01
	bne pattern14_third
	lda stopright
	cmp #$01
	bne pattern14_third
	jsr comp_topright
	jmp wait_turn
pattern14_third:
	lda atopleft
	cmp #$01
	bne pattern14_fourth
	lda atop
	cmp #$01
	bne pattern14_fourth
	lda xtopright
	cmp #$01
	bne pattern14_fourth
	lda aleft
	cmp #$01
	bne pattern14_fourth
	lda omiddle
	cmp #$01
	bne pattern14_fourth
	lda sright
	cmp #$01
	bne pattern14_fourth
	lda xbottomleft
	cmp #$01
	bne pattern14_fourth
	lda sbottom
	cmp #$01
	bne pattern14_fourth
	lda sbottomright
	cmp #$01
	bne pattern14_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern14_fourth:
	lda xtopleft
	cmp #$01
	bne pattern15_first
	lda atop
	cmp #$01
	bne pattern15_first
	lda atopright
	cmp #$01
	bne pattern15_first
	lda sleft
	cmp #$01
	bne pattern15_first
	lda omiddle
	cmp #$01
	bne pattern15_first
	lda aright
	cmp #$01
	bne pattern15_first
	lda xbottomright
	cmp #$01
	bne pattern15_first
	lda sbottom
	cmp #$01
	bne pattern15_first
	lda sbottomleft
	cmp #$01
	bne pattern15_first
	jsr comp_bottomleft
	jmp wait_turn

pattern15_first:
	lda atop
	cmp #$01
	bne pattern15_second
	lda atopright
	cmp #$01
	bne pattern15_second
	lda aleft
	cmp #$01
	bne pattern15_second
	lda xmiddle
	cmp #$01
	bne pattern15_second
	lda aright
	cmp #$01
	bne pattern15_second
	lda abottomleft
	cmp #$01
	bne pattern15_second
	lda abottom
	cmp #$01
	bne pattern15_second
	lda sbottomright
	cmp #$01
	bne pattern15_second
	lda stopleft
	cmp #$01
	bne pattern15_second
	jsr comp_topleft
	jmp wait_turn
pattern15_second:
	lda atopleft
	cmp #$01
	bne pattern15_third
	lda atop
	cmp #$01
	bne pattern15_third
	lda aleft
	cmp #$01
	bne pattern15_third
	lda xmiddle
	cmp #$01
	bne pattern15_third
	lda aright
	cmp #$01
	bne pattern15_third
	lda sbottomleft
	cmp #$01
	bne pattern15_third
	lda abottom
	cmp #$01
	bne pattern15_third
	lda abottomright
	cmp #$01
	bne pattern15_third
	lda stopright
	cmp #$01
	bne pattern15_third
	jsr comp_topright
	jmp wait_turn
pattern15_third:
	lda stopleft
	cmp #$01
	bne pattern15_fourth
	lda atop
	cmp #$01
	bne pattern15_fourth
	lda atopright
	cmp #$01
	bne pattern15_fourth
	lda aleft
	cmp #$01
	bne pattern15_fourth
	lda xmiddle
	cmp #$01
	bne pattern15_fourth
	lda aright
	cmp #$01
	bne pattern15_fourth
	lda abottomleft
	cmp #$01
	bne pattern15_fourth
	lda abottom
	cmp #$01
	bne pattern15_fourth
	lda sbottomright
	cmp #$01
	bne pattern15_fourth
	jsr comp_bottomright
	jmp wait_turn
pattern15_fourth:
	lda atopleft
	cmp #$01
	bne pattern16_first
	lda atop
	cmp #$01
	bne pattern16_first
	lda stopright
	cmp #$01
	bne pattern16_first
	lda aleft
	cmp #$01
	bne pattern16_first
	lda xmiddle
	cmp #$01
	bne pattern16_first
	lda aright
	cmp #$01
	bne pattern16_first
	lda abottom
	cmp #$01
	bne pattern16_first
	lda abottomright
	cmp #$01
	bne pattern16_first
	lda sbottomleft
	cmp #$01
	bne pattern16_first
	jsr comp_bottomleft
	jmp wait_turn

pattern16_first:
	lda atopleft
	cmp #$01
	bne pattern16_second
	lda atopright
	cmp #$01
	bne pattern16_second
	lda aleft
	cmp #$01
	bne pattern16_second
	lda xmiddle
	cmp #$01
	bne pattern16_second
	lda aright
	cmp #$01
	bne pattern16_second
	lda abottomleft
	cmp #$01
	bne pattern16_second
	lda sbottom
	cmp #$01
	bne pattern16_second
	lda abottomright
	cmp #$01
	bne pattern16_second
	lda stop
	cmp #$01
	bne pattern16_second
	jsr comp_top
	jmp wait_turn
pattern16_second:
	lda atopleft
	cmp #$01
	bne pattern16_third
	lda atop
	cmp #$01
	bne pattern16_third
	lda atopright
	cmp #$01
	bne pattern16_third
	lda xmiddle
	cmp #$01
	bne pattern16_third
	lda sleft
	cmp #$01
	bne pattern16_third
	lda abottomleft
	cmp #$01
	bne pattern16_third
	lda abottom
	cmp #$01
	bne pattern16_third
	lda abottomright
	cmp #$01
	bne pattern16_third
	lda sright
	cmp #$01
	bne pattern16_third
	jsr comp_right
	jmp wait_turn
pattern16_third:
	lda atopleft
	cmp #$01
	bne pattern16_fourth
	lda stop
	cmp #$01
	bne pattern16_fourth
	lda atopright
	cmp #$01
	bne pattern16_fourth
	lda aleft
	cmp #$01
	bne pattern16_fourth
	lda xmiddle
	cmp #$01
	bne pattern16_fourth
	lda aright
	cmp #$01
	bne pattern16_fourth
	lda abottomleft
	cmp #$01
	bne pattern16_fourth
	lda abottomright
	cmp #$01
	bne pattern16_fourth
	lda sbottom
	cmp #$01
	bne pattern16_fourth
	jsr comp_bottom
	jmp wait_turn
pattern16_fourth:
	lda atopleft
	cmp #$01
	bne try_middle
	lda atop
	cmp #$01
	bne try_middle
	lda atopright
	cmp #$01
	bne try_middle
	lda xmiddle
	cmp #$01
	bne try_middle
	lda sright
	cmp #$01
	bne try_middle
	lda abottomleft
	cmp #$01
	bne try_middle
	lda abottom
	cmp #$01
	bne try_middle
	lda abottomright
	cmp #$01
	bne try_middle
	lda sleft
	cmp #$01
	bne try_middle
	jsr comp_left
	jmp wait_turn


try_middle:
	lda smiddle
	cmp #$01
	bne try_topleft
	jsr comp_middle
	jmp wait_turn
try_topleft:
	lda stopleft
	cmp #$01
	bne try_topright
	jsr comp_topleft
	jmp wait_turn
try_topright:
	lda stopright
	cmp #$01
	bne try_bottomleft
	jsr comp_topright
	jmp wait_turn
try_bottomleft:
	lda sbottomleft
	cmp #$01
	bne try_bottomright
	jsr comp_bottomleft
	jmp wait_turn
try_bottomright:
	lda sbottomright
	cmp #$01
	bne try_top
	jsr comp_bottomright
	jmp wait_turn
try_top:
	lda stop
	cmp #$01
	bne try_left
	jsr comp_top
	jmp wait_turn
try_left:
	lda sleft
	cmp #$01
	bne try_right
	jsr comp_left
	jmp wait_turn
try_right:
	lda sright
	cmp #$01
	bne try_bottom
	jsr comp_right
	jmp wait_turn
try_bottom:
	lda sbottom
	cmp #$01
	bne end_computer
	jsr comp_bottom
	jmp wait_turn

end_computer:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	lda #$21
	sta $2006
	lda #$0b
	sta $2006
	lda stalemate
:	sta $2007
	inx
	lda stalemate,x
	cpx #$0a
	bne :-
	lda stalecount
	inc stalecount
:	bit $2002
	bpl :-
   	ldx #$00
	lda #$21
	sta $2006
	lda #$2b
	sta $2006
	lda blank_under_stale
:	sta $2007
	inx
	lda blank_under_stale,x
	cpx #$0a
	bne :-
:	bit $2002
	bpl :-
	lda #$00
	sta $2006
	sta $2006
	lda whose_turn
	cmp #$00
	bne pass_whose_turn
	lda #$01
	sta whose_turn
pass_whose_turn:
	jsr PPU_no_sprites
	jsr fix_board
	jsr waste_all_time
	jsr clear_topleft
	jsr clear_top
	jsr clear_topright
	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr PPU_no_sprites
	jsr waste_all_time
	jsr clear_left
	jsr clear_middle
	jsr clear_right
	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr PPU_no_sprites
	jsr waste_all_time
	jsr clear_bottomleft
	jsr clear_bottom
	jsr clear_bottomright

	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr PPU_with_sprites
	lda player_won
	cmp #$01
	beq clear_for_player_win
	jsr waste_all_time
	lda #$2						; Load song 3
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000

	jmp reinit_variables
clear_for_player_win:
	rts

;loop:
;	jmp loop

PPU_off:
	lda #$00
	sta $2000
	sta $2001
	rts

PPU_with_sprites:
	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001
	rts

PPU_no_sprites:
	lda #%10000000
	sta $2000
	lda #%00001110
	sta $2001
	rts

clear_slots:
clear_topleft:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$20
	sta $2006
	lda filltopleft,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_top:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$20
	sta $2006
	lda filltop,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_topright:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$20
	sta $2006
	lda filltopright,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_left:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$21
	sta $2006
	lda fillleft,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_middle:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$21
	sta $2006
	lda fillmiddle,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_right:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$21
	sta $2006
	lda fillright,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_bottomleft:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$22
	sta $2006
	lda fillbottomleft,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_bottom:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$22
	sta $2006
	lda fillbottom,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts
clear_bottomright:
:	bit $2002
	bpl :-
	ldx #$00
	stx $2000
	stx $2001
:	lda #$22
	sta $2006
	lda fillbottomright,x
	sta $2006
	lda clear_them,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$00
	sta $2006
	sta $2006
	rts

fix_board:
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	lda #$21
	sta $2006
	lda #$0b
	sta $2006
	lda fix_message
:	sta $2007
	inx
	lda fix_message,x
	cpx #$0a
	bne :-
	lda #$00
	sta $2006
	sta $2006
:	bit $2002
	bpl :-
   	ldx #$00
	lda #$21
	sta $2006
	lda #$2b
	sta $2006
	lda under_fix_message
:	sta $2007
	inx
	lda under_fix_message,x
	cpx #$0a
	bne :-
:	bit $2002
	bpl :-
	lda #$00
	sta $2006
	sta $2006
	jsr PPU_no_sprites
	rts

waste_all_time:
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	ldy #$60
:	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	ldx #$60
:	stx comp_think
	dex
	cpx #$00
	bne :-
	dey
	cpy #$00
	bne :---
	rts

comp_topleft:
	jsr waste_all_time
   	ldx #$00
	stx $2000
	stx $2001
:	lda #$20
	sta $2006
	lda filltopleft,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta otopleft
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta stopleft
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_top:
	jsr waste_all_time
	lda #$00
	sta $2000
	sta $2001
	ldx #$00
:	lda #$20
	sta $2006
	lda filltop,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta otop
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta stop
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_topright:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$20
	sta $2006
	lda filltopright,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta otopright
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta stopright
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_left:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$21
	sta $2006
	lda fillleft,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta oleft
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta sleft
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_middle:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$21
	sta $2006
	lda fillmiddle,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta omiddle
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta smiddle
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_right:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$21
	sta $2006
	lda fillright,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta oright
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta sright
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_bottomleft:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomleft,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta obottomleft
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta sbottomleft
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_bottom:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottom,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta obottom
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta sbottom
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

comp_bottomright:
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomright,x
	sta $2006
	lda o_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta obottomright
	sta whose_turn
:	bit $2002
	bpl :-
	lda move_count
	inc move_count
	lda #$00
	sta sbottomright
	sta $2006
	sta $2006
	jsr PPU_with_sprites
	rts

computer_win:
	lda #$00
	sta whose_turn
	ldx #$00
	stx $2000
	stx $2001
	jsr PPU_no_sprites

	lda #$4						; Load song 5
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr waste_all_time
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	ldx #$00
:	lda #$21
	sta $2006
	lda fillmiddle,x
	sta $2006
	lda game_over,x
	sta $2007
	inx
	cpx #$10
	bne :-
:	bit $2002
	bpl :-
	lda #$00
	sta $2006
	sta $2006
	jsr PPU_no_sprites
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time

	jmp reset

test_x_win:
	lda xtopleft
	beq test2
	lda xtop
	beq test2
	lda xtopright
	beq test2
	lda aleft
	beq test2
	lda amiddle
	beq test2
	lda aright
	beq test2
	lda abottomleft
	beq test2
	lda abottom
	beq test2
	lda abottomright
	beq test2
	lda horiz_win
	inc horiz_win
	jmp youwin_words
test2:
	lda xtopleft
	beq test3
	lda atop
	beq test3
	lda atopright
	beq test3
	lda aleft
	beq test3
	lda xmiddle
	beq test3
	lda aright
	beq test3
	lda abottomleft
	beq test3
	lda abottom
	beq test3
	lda xbottomright
	beq test3
	lda diag_win
	inc diag_win
	jmp youwin_words
test3:
	lda xtopleft
	beq test4
	lda atop
	beq test4
	lda atopright
	beq test4
	lda xleft
	beq test4
	lda amiddle
	beq test4
	lda aright
	beq test4
	lda xbottomleft
	beq test4
	lda abottom
	beq test4
	lda abottomright
	beq test4
	lda vert_win
	inc vert_win
	jmp youwin_words
test4:
	lda atopleft
	beq test5
	lda atop
	beq test5
	lda atopright
	beq test5
	lda xleft
	beq test5
	lda xmiddle
	beq test5
	lda xright
	beq test5
	lda abottomleft
	beq test5
	lda abottom
	beq test5
	lda abottomright
	beq test5
	lda horiz_win
	inc horiz_win
	jmp youwin_words
test5:
	lda atopleft
	beq test6
	lda atop
	beq test6
	lda atopright
	beq test6
	lda aleft
	beq test6
	lda amiddle
	beq test6
	lda aright
	beq test6
	lda xbottomleft
	beq test6
	lda xbottom
	beq test6
	lda xbottomright
	beq test6
	lda horiz_win
	inc horiz_win
	jmp youwin_words
test6:
	lda atopleft
	beq test7
	lda atop
	beq test7
	lda xtopright
	beq test7
	lda aleft
	beq test7
	lda xmiddle
	beq test7
	lda aright
	beq test7
	lda xbottomleft
	beq test7
	lda abottom
	beq test7
	lda abottomright
	beq test7
	lda diag_win
	inc diag_win
	jmp youwin_words
test7:
	lda atopleft
	beq test8
	lda xtop
	beq test8
	lda atopright
	beq test8
	lda aleft
	beq test8
	lda xmiddle
	beq test8
	lda aright
	beq test8
	lda abottomleft
	beq test8
	lda xbottom
	beq test8
	lda abottomright
	beq test8
	lda vert_win
	inc vert_win
	jmp youwin_words
test8:
	lda atopleft
	beq not_win
	lda atop
	beq not_win
	lda xtopright
	beq not_win
	lda aleft
	beq not_win
	lda amiddle
	beq not_win
	lda xright
	beq not_win
	lda abottomleft
	beq not_win
	lda abottom
	beq not_win
	lda xbottomright
	beq not_win
	lda vert_win
	inc vert_win
	jmp youwin_words

not_win:
	rts
	
youwin_words:
	lda #$00
;	sta $2000
;	sta $2001
	sta $2006
	sta $2006
	sta $2005
	sta $2005
	lda #$01
	sta player_won
	jsr PPU_no_sprites
	jsr waste_all_time
	ldx #$00
	stx $2000
	stx $2001
	lda #$21
	sta $2006
	lda #$0b
	sta $2006
	lda youwin
:	sta $2007
	inx
	lda youwin,x
	cpx #$0a
	bne :-
:	bit $2002
	bpl :-
   	ldx #$00
	lda #$21
	sta $2006
	lda #$2b
	sta $2006
	lda blank_under_stale
:	sta $2007
	inx
	lda blank_under_stale,x
	cpx #$0a
	bne :-
:	bit $2002
	bpl :-
	lda #$00
	sta $2006
	sta $2006
	lda #$01
	sta whose_turn
	jsr PPU_no_sprites
	jsr fix_board
	jsr waste_all_time
	jsr clear_topleft
	jsr clear_top
	jsr clear_topright
	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr PPU_no_sprites
	jsr waste_all_time
	jsr clear_left
	jsr clear_middle
	jsr clear_right
	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr PPU_no_sprites
	jsr waste_all_time
	jsr clear_bottomleft
	jsr clear_bottom
	jsr clear_bottomright

	lda #$3						; Load song 4
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	lda inc_win
	inc inc_win
check_comp_lose_whole_game:
	lda inc_win
	cmp #$05
	bne continue_game_more
	jmp computer_beaten_now
continue_game_more:
	jsr PPU_with_sprites
;	lda player_won
;	cmp #$01
;	beq clear_for_player_win
	jsr waste_all_time

	lda #$2						; Load song 3
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jmp reinit_variables

computer_beaten_now:
	jsr waste_all_time
	jsr PPU_off
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
	sta no_control
	ldy #$00					; load game screen
	ldx #$04
	lda #<beat_it
	sta $10
	lda #>beat_it
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
	jsr PPU_no_sprites
	lda #$5						; Load song 6
	ldx #$00
	jsr init
	lda #%10000000
	sta $2000
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jsr waste_all_time
	jmp reset
; *********************************************************
; The control routine is below. The whole routine is      *
; triggered inside of NMI.                                *
; *********************************************************
play2:
	lda #$01
	sta $4017
	lda #$00
	sta $4017
	lda control_pad2
	sta control_old2
	ldx #$08
:
	lda $4017
	lsr A
	ror control_pad2
	dex
	bne :-

	lda control_pad2				; Right button check/routine
	eor control_old2
	and control_pad2
	and #right_punch
	beq no_play2
	jsr hidden_thanks
no_play2:
	rts
controller:
	lda #$01
	sta $4016
	lda #$00
	sta $4016
	lda control_pad
	sta control_old
	ldx #$08
:
	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-

	lda control_pad				; Right button check/routine
	eor control_old
	and control_pad
	and #right_punch
	beq no_right_punch
	lda game_start
	beq skip_right_effects
	ldx #$04
	stx $4015
	ldx #$00
	jsr fx_routine
skip_right_effects:
	lda game_start
	bne no_rightshift	 		; YOU ARE HERE
	lda #$01
	sta instr_switch
	jmp no_right_punch
no_rightshift:
	lda doing_item3
	bne no_right_punch
	lda #$c0					
	cmp sprite1+3				
	bne keep_right_going		
	lda #$38					
	sta sprite+3
	lda #$40
	sta sprite1+3				
	cmp #$40					
	beq no_right_punch			
keep_right_going:				
	clc
	lda sprite+3
	adc #$40
	sta sprite+3
	clc
	lda sprite1+3
	adc #$40
	sta sprite1+3
no_right_punch:
	lda control_pad				; Left button check/routine
	eor control_old
	and control_pad
	and #left_punch
	beq no_left_punch
	lda game_start
	bne skip_left_effects
	ldx #$04
	stx $4015
	ldx #$00
	jsr fx_routine
skip_left_effects:
	lda game_start
	bne no_leftshift	 		; YOU ARE HERE
	lda #$01
	sta instr_switch2
	jmp no_left_punch
no_leftshift:
	lda doing_item3
	bne no_left_punch
	lda #$40					
	cmp sprite1+3				
	bne keep_left_going		
	lda #$b8					
	sta sprite+3
	lda #$c0
	sta sprite1+3
	cmp #$c0					
	beq no_left_punch			
keep_left_going:				
	sec
	lda sprite+3
	sbc #$40
	sta sprite+3
	sec
	lda sprite1+3
	sbc #$40
	sta sprite1+3
no_left_punch:
	lda control_pad				; Down button check/routine
	eor control_old
	and control_pad
	and #down_punch
	beq no_down_punch
	lda game_start
	beq skip_down_effects
	ldx #$04
	stx $4015
	ldx #$00
	jsr fx_routine
skip_down_effects:
	lda doing_item4
	bne no_down_punch
	lda #$8d					
	cmp sprite1				
	bne keep_down_going		
	lda #$0d					
	sta sprite
	lda #$0d
	sta sprite1				
	cmp #$0d					
	beq no_down_punch			
keep_down_going:				
	clc
	lda sprite
	adc #$40
	sta sprite
	clc
	lda sprite1
	adc #$40
	sta sprite1
no_down_punch:
	lda control_pad				; Up button check/routine
	eor control_old
	and control_pad
	and #up_punch
	beq no_up_punch
	lda game_start
	bne skip_up_effects
	ldx #$04
	stx $4015
	ldx #$00
	jsr fx_routine
skip_up_effects:
	lda doing_item4
	bne no_up_punch
	lda #$0d					
	cmp sprite1				
	bne keep_up_going		
	lda #$8d					
	sta sprite
	lda #$8d
	sta sprite1				
	cmp #$8d					
	beq no_up_punch			
keep_up_going:				
	sec
	lda sprite
	sbc #$40
	sta sprite
	sec
	lda sprite1
	sbc #$40
	sta sprite1
no_up_punch:
	lda control_pad				; Start button check/routine
	eor control_old
	and control_pad
	and #start_punch
	beq no_start_punch
	lda cant_see_hidden
	cmp #$01
	bne check_game_start
	lda game_start
	bne no_start_punch
	jmp reset
check_game_start:
	lda #$01
	sta game_start
no_start_punch:
	lda control_pad				; Select button check/routine
	eor control_old
	and control_pad
	and #select_punch
	beq no_select_punch
	lda game_start
	beq skip_select_effects
	ldx #$04
	stx $4015
	ldx #$04
	jsr fx_routine
skip_select_effects:
	lda game_start
	bne no_instructions			; YOU ARE HERE
	lda #$01
	sta title_select
	jmp no_select_punch
no_instructions:
	lda #$c0					; Load A with the last position that
	cmp sprite2+3				; the item cursor is allowed to be at.
	bne keep_select_going		; If it's there already, load A with
	lda #$30					; the first allowed cursor position,
	sta sprite2+3				; and do a comparison to be allowed to
	cmp #$30					; jump over the rest of the code for
	beq no_select_punch			; the select button.
keep_select_going:				; If it's not at the last allowed pos,
	clc							; add #$30 to the X-Pos of the item
	lda sprite2+3				; selection sprite, and store that
	adc #$30					; value back into the X-Pos of the
	sta sprite2+3				; sprite
no_select_punch:
	lda control_pad				; B button check/routine
	eor control_old
	and control_pad
	and #b_punch
	beq fake_no_B_punch1
	lda no_control
	bne fake_no_B_punch1
	lda sprite2+3
	cmp #$30
	bne check_second_pos
	lda #$2f
	cmp sprite3+1
	beq fake_no_B_punch1
	lda #$02
	sta whose_turn
	lda #$2f
	sta sprite3+1
	lda #$00
	sta stalecount
	jmp no_b_punch
check_second_pos:
	lda sprite2+3
	cmp #$60
	bne fake_third_pos_jump1
	lda #$2f
	cmp sprite5+1
	beq fake_no_B_punch1

	lda sprite
	cmp #$0d
	beq check_for_top_row
	lda sprite
	cmp #$4d
	beq fake_check_for_middle_row
	lda sprite
	cmp #$8d
	beq fake_check_for_bottom_row

fake_no_B_punch1:
	jmp no_b_punch

check_for_top_row:
	lda sprite+3
	cmp #$38
	beq check_topleft_o
	lda sprite+3
	cmp #$78
	beq check_top_o
	lda sprite+3
	cmp #$b8
	beq fake_check_topright_o
check_topleft_o:
	lda otopleft
	beq fake_no_B_punch1
	ldx #$00
:	lda #$20
	sta $2006
	lda filltopleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtopleft
	lda #$00
	sta stopleft
	sta otopleft
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn1
	lda whose_turn
	dec whose_turn
skip_that_turn1:
	jmp no_b_punch

fake_check_for_middle_row:
	jmp check_for_middle_row
fake_third_pos_jump1:
	jmp check_third_pos
fake_check_for_bottom_row:
	jmp check_for_bottom_row
fake_check_topright_o:
	jmp check_topright_o
check_top_o:
	lda otop
	beq fake_no_B_punch1
	ldx #$00
:	lda #$20
	sta $2006
	lda filltop,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtop
	lda #$00
	sta stop
	sta otop
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn2
	lda whose_turn
	dec whose_turn
skip_that_turn2:
	jmp no_b_punch
check_topright_o:
	lda otopright
	beq fake_no_B_punch2
	ldx #$00
:	lda #$20
	sta $2006
	lda filltopright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtopright
	lda #$00
	sta stopright
	sta otopright
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn3
	lda whose_turn
	dec whose_turn
skip_that_turn3:
	jmp no_b_punch
check_for_middle_row:
	lda sprite+3
	cmp #$38
	beq check_left_o
	lda sprite+3
	cmp #$78
	beq check_middle_o
	lda sprite+3
	cmp #$b8
	beq fake_check_right_o

fake_no_B_punch2:
	jmp no_b_punch

check_left_o:
	lda oleft
	beq fake_no_B_punch2
	ldx #$00
:	lda #$21
	sta $2006
	lda fillleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xleft
	lda #$00
	sta sleft
	sta oleft
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn4
	lda whose_turn
	dec whose_turn
skip_that_turn4:
	jmp no_b_punch

fake_check_right_o:
	jmp check_right_o

check_middle_o:
	lda omiddle
	beq fake_no_B_punch2
	ldx #$00
:	lda #$21
	sta $2006
	lda fillmiddle,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xmiddle
	lda #$00
	sta smiddle
	sta omiddle
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn5
	lda whose_turn
	dec whose_turn
skip_that_turn5:
	jmp no_b_punch
check_right_o:
	lda oright
	beq fake_no_B_punch3
	ldx #$00
:	lda #$21
	sta $2006
	lda fillright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xright
	lda #$00
	sta sright
	sta oright
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn6
	lda whose_turn
	dec whose_turn
skip_that_turn6:
	jmp no_b_punch

fake_no_B_punch3:
	jmp no_b_punch

check_for_bottom_row:
	lda sprite+3
	cmp #$38
	beq check_bottomleft_o
	lda sprite+3
	cmp #$78
	beq check_bottom_o
	lda sprite+3
	cmp #$b8
	beq fake_check_bottomright_o
check_bottomleft_o:
	lda obottomleft
	beq fake_no_B_punch3
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottomleft
	lda #$00
	sta sbottomleft
	sta obottomleft
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn7
	lda whose_turn
	dec whose_turn
skip_that_turn7:
	jmp no_b_punch
fake_check_bottomright_o:
	jmp check_bottomright_o
check_bottom_o:
	lda obottom
	beq fake_no_B_punch3
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottom,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottom
	lda #$00
	sta sbottom
	sta obottom
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn8
	lda whose_turn
	dec whose_turn
skip_that_turn8:
	jmp no_b_punch
check_bottomright_o:
	lda obottomright
	beq skip_that_turn9
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottomright
	lda #$00
	sta sbottomright
	sta obottomright
	sta switch_to_x
	sta diag_win
	lda #$2f
	sta sprite5+1
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_that_turn9
	lda whose_turn
	dec whose_turn
skip_that_turn9:
	jmp no_b_punch

check_third_pos:
	lda sprite2+3
	cmp #$90
	bne check_fourth_pos
	lda horiz_win
	cmp #$02
	bne no_b_punch
	lda #$01
	sta doing_item3
	lda #$0d
	sta sprite
	sta sprite1
	lda #$38
	sta sprite+3
	lda #$40
	sta sprite1+3
	lda #$2f
	sta sprite7+1
	lda #$00
	sta clear_horiz				; MIGHT BE IFFY ROB!!!
	sta horiz_win
	jmp no_b_punch
check_fourth_pos:
	lda sprite2+3
	cmp #$c0
	bne no_b_punch
	lda vert_win				; YOU'RE HERE ROB!!!
	cmp #$02
	bne no_b_punch
	lda #$01
	sta doing_item4
	lda #$0d
	sta sprite
	sta sprite1
	lda #$38
	sta sprite+3
	lda #$40
	sta sprite1+3
	lda #$2f
	sta sprite9+1
	lda #$00
	sta clear_vert				; MIGHT BE IFFY ROB!!!
	sta vert_win
	jmp no_b_punch
no_b_punch:
	lda control_pad				; A button check/routine
	eor control_old
	and control_pad
	and #a_punch	
	beq fake_no_a_punch
	
	lda doing_item3
	cmp #$01
	bne fake_test_item4
	lda sprite1
	cmp #$0d
	beq clear_that_top_row
	lda sprite1
	cmp #$4d
	beq clear_that_middle_row
	lda sprite1
	cmp #$8d
	beq fake_clear_that_bottom_row
	jmp test_item4
fake_test_item4:
	jmp test_item4
fake_no_a_punch:
	jmp no_a_punch
fake_regular_placement:
	jmp regular_placement
fake_clear_that_middle_column:
	jmp clear_that_middle_column
fake_clear_that_right_column:
	jmp clear_that_right_column
fake_clear_that_bottom_row:
	jmp clear_that_bottom_row
clear_that_top_row:
	lda stopleft
	bne not_topleft_row
	lda #$00
	sta otopleft
	sta xtopleft
	lda #$01
	sta stopleft
	jsr clear_topleft
	lda move_count
	dec move_count
not_topleft_row:
	lda stop
	bne not_top_row
	lda #$00
	sta otop
	sta xtop
	lda #$01
	sta stop
	jsr clear_top
	lda move_count
	dec move_count
not_top_row:
	lda stopright
	bne wasted_top_row_turn
	lda #$00
	sta otopright
	sta xtopright
	lda #$01
	sta stopright
	jsr clear_topright
	lda move_count
	dec move_count
wasted_top_row_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item3
	sta $2005
	sta $2005
	jmp no_a_punch
clear_that_middle_row:
	lda sleft
	bne not_left_row
	lda #$00
	sta oleft
	sta xleft
	lda #$01
	sta sleft
	jsr clear_left
	lda move_count
	dec move_count
not_left_row:
	lda smiddle
	bne not_middle_row
	lda #$00
	sta omiddle
	sta xmiddle
	lda #$01
	sta smiddle
	jsr clear_middle
	lda move_count
	dec move_count
not_middle_row:
	lda sright
	bne wasted_middle_row_turn
	lda #$00
	sta oright
	sta xright
	lda #$01
	sta sright
	jsr clear_right
	lda move_count
	dec move_count
wasted_middle_row_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item3
	sta $2005
	sta $2005
	jmp no_a_punch
clear_that_bottom_row:
	lda sbottomleft
	bne not_bottomleft_row
	lda #$00
	sta obottomleft
	sta xbottomleft
	lda #$01
	sta sbottomleft
	jsr clear_bottomleft
	lda move_count
	dec move_count
not_bottomleft_row:
	lda sbottom
	bne not_bottom_row
	lda #$00
	sta obottom
	sta xbottom
	lda #$01
	sta sbottom
	jsr clear_bottom
	lda move_count
	dec move_count
not_bottom_row:
	lda sbottomright
	bne wasted_bottom_row_turn
	lda #$00
	sta obottomright
	sta xbottomright
	lda #$01
	sta sbottomright
	jsr clear_bottomright
	lda move_count
	dec move_count
wasted_bottom_row_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item3
	sta $2005
	sta $2005
	jmp no_a_punch
fake_regular_placement2:
	jmp regular_placement
fake_clear_that_middle_column2:
	jmp clear_that_middle_column
fake_clear_that_right_column2:
	jmp clear_that_right_column
test_item4:
	lda doing_item4
	cmp #$01
	bne fake_regular_placement2
	lda sprite+3
	cmp #$38
	beq clear_that_left_column
	lda sprite+3
	cmp #$78
	beq fake_clear_that_middle_column2
	lda sprite+3
	cmp #$b8
	beq fake_clear_that_right_column2
	jmp regular_placement
clear_that_left_column:				; THIS IS THE BUGGY SHIT ROB!!!
	lda stopleft
	bne not_top_left_column
	lda #$00
	sta otopleft
	sta xtopleft
	lda #$01
	sta stopleft
	jsr clear_topleft
	lda move_count
	dec move_count
not_top_left_column:
	lda sleft
	bne not_left_column
	lda #$00
	sta oleft
	sta xleft
	lda #$01
	sta sleft
	jsr clear_left
	lda move_count
	dec move_count
not_left_column:
	lda sbottomleft
	bne wasted_left_column_turn
	lda #$00
	sta obottomleft
	sta xbottomleft
	lda #$01
	sta sbottomleft
	jsr clear_bottomleft
	lda move_count
	dec move_count
wasted_left_column_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item4
	sta $2005
	sta $2005
	jmp no_a_punch
clear_that_middle_column:
	lda stop
	bne not_top_column
	lda #$00
	sta otop
	sta xtop
	lda #$01
	sta stop
	jsr clear_top
	lda move_count
	dec move_count
not_top_column:
	lda smiddle
	bne not_middle_column
	lda #$00
	sta omiddle
	sta xmiddle
	lda #$01
	sta smiddle
	jsr clear_middle
	lda move_count
	dec move_count
not_middle_column:
	lda sbottom
	bne wasted_middle_column_turn
	lda #$00
	sta obottom
	sta xbottom
	lda #$01
	sta sbottom
	jsr clear_bottom
	lda move_count
	dec move_count
wasted_middle_column_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item4
	sta $2005
	sta $2005
	jmp no_a_punch
clear_that_right_column:
	lda stopright
	bne not_topright_column
	lda #$00
	sta otopright
	sta xtopright
	lda #$01
	sta stopright
	jsr clear_topright
	lda move_count
	dec move_count
not_topright_column:
	lda sright
	bne not_right_column
	lda #$00
	sta oright
	sta xright
	lda #$01
	sta sright
	jsr clear_right
	lda move_count
	dec move_count
not_right_column:
	lda sbottomright
	bne wasted_right_column_turn
	lda #$00
	sta obottomright
	sta xbottomright
	lda #$01
	sta sbottomright
	jsr clear_bottomright
	lda move_count
	dec move_count
wasted_right_column_turn:
	lda whose_turn
	dec whose_turn
	lda #$00
	sta doing_item4
	sta $2005
	sta $2005
	jmp no_a_punch
fake_no_A_jump3:
	jmp no_a_punch
regular_placement:
	lda sprite
	cmp #$0d
	beq top_row_compare
	lda sprite
	cmp #$4d
	beq fake_middle_compare
	lda sprite
	cmp #$8d
	beq fake_bottom_compare

top_row_compare:
	lda sprite+3
	cmp #$38
	beq top_left_go
	lda sprite+3
	cmp #$78
	beq top_go
	lda sprite+3
	cmp #$b8
	beq top_right_go

fake_bottom_compare:
	jmp bottom_row_compare

top_left_go:
	lda stopleft
	cmp #$00
	beq fake_no_A_jump1
	ldx #$00
:	lda #$20
	sta $2006
	lda filltopleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtopleft
	lda #$00
	sta stopleft
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn1
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn1:
	jmp no_a_punch

fake_no_A_jump1:
	jmp fake_no_A_jump2

fake_middle_compare:
	jmp middle_row_compare

top_go:
	lda stop
	cmp #$00
	beq fake_no_A_jump1
	ldx #$00
:	lda #$20
	sta $2006
	lda filltop,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtop
	lda #$00
	sta stop
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn2
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn2:
	jmp no_a_punch

top_right_go:
	lda stopright
	cmp #$00
	beq fake_no_A_jump1
	ldx #$00
:	lda #$20
	sta $2006
	lda filltopright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xtopright
	lda #$00
	sta stopright
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn3
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn3:
	jmp no_a_punch

middle_row_compare:
	lda sprite+3
	cmp #$38
	beq left_go
	lda sprite+3
	cmp #$78
	beq middle_go
	lda sprite+3
	cmp #$b8
	beq right_go

left_go:
	lda sleft
	cmp #$00
	beq fake_no_A_jump2
	ldx #$00
:	lda #$21
	sta $2006
	lda fillleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xleft
	lda #$00
	sta sleft
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn4
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn4:
	jmp no_a_punch

middle_go:
	lda smiddle
	cmp #$00
	beq fake_no_A_jump2
	ldx #$00
:	lda #$21
	sta $2006
	lda fillmiddle,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xmiddle
	lda #$00
	sta smiddle
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn5
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn5:
	jmp no_a_punch

fake_no_A_jump2:
	jmp no_a_punch

right_go:
	lda sright
	cmp #$00
	beq fake_no_A_jump2
	ldx #$00
:	lda #$21
	sta $2006
	lda fillright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xright
	lda #$00
	sta sright
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn6
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn6:
	jmp no_a_punch
	
bottom_row_compare:
	lda sprite+3
	cmp #$38
	beq bottom_left_go
	lda sprite+3
	cmp #$78
	beq bottom_go
	lda sprite+3
	cmp #$b8
	beq bottom_right_go

bottom_left_go:
	lda sbottomleft
	cmp #$00
	beq fake_no_A_jump2
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomleft,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottomleft
	lda #$00
	sta sbottomleft
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn7
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn7:
	jmp no_a_punch

bottom_go:
	lda sbottom
	cmp #$00
	beq no_a_punch
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottom,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottom
	lda #$00
	sta sbottom
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn8
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn8:
	jmp no_a_punch

bottom_right_go:
	lda sbottomright
	cmp #$00
	beq no_a_punch
	ldx #$00
:	lda #$22
	sta $2006
	lda fillbottomright,x
	sta $2006
	lda x_tiles,x
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$01
	sta xbottomright
	lda #$00
	sta sbottomright
	jsr test_x_win
	lda player_won
	cmp #$01
	beq skip_turn9
	lda whose_turn
	dec whose_turn
	lda move_count
	inc move_count
skip_turn9:
	jmp no_a_punch

no_a_punch:
	rts

cant_place_x:
	jsr fx_routine

hidden_thanks:
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
;	sta no_control
	ldy #$00					; load game screen
	ldx #$04
	lda #<thanks
	sta $10
	lda #>thanks
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
	jsr PPU_no_sprites
	lda #$01
	sta cant_see_hidden
	rts

fx_routine:
	lda sound_fx,x
	sta $4000
	inx
	lda sound_fx,x
	sta $4001
	inx
	lda sound_fx,x
	sta $4002
	inx
	lda sound_fx,x
	sta $4003
	inx
	rts

instructions0:
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
;	sta no_control
	ldy #$00					; load game screen
	ldx #$04
	lda #<instr0
	sta $10
	lda #>instr0
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
	lda #$01
	sta cant_see_hidden
	jsr PPU_no_sprites
hold_instr0:
	lda game_start
	cmp #$01
	bne check_switch1
	jmp keep_holding_title
check_switch1:
	lda instr_switch
	cmp #$01
	bne keep_holding_instructions
	lda #$00
	sta instr_switch2
	jmp instructions1
keep_holding_instructions:
	lda instr_hold
	cmp #$01
	bne hold_instr0


instructions1:
	lda #$00					; turn off PPU
	sta $2000
	sta $2001
;	sta no_control
	ldy #$00					; load game screen
	ldx #$04
	lda #<instr1
	sta $10
	lda #>instr1
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-
	lda #$01
	sta cant_see_hidden
	jsr PPU_with_sprites
hold_instr1:
	lda game_start
	cmp #$01
	bne check_switch2
	jmp keep_holding_title
check_switch2:
	lda instr_switch2
	cmp #$01
	bne keep_holding_instructions2
	lda #$00
	sta instr_switch
	jmp instructions0
keep_holding_instructions2:
	lda instr_hold
	cmp #$01
	bne hold_instr1
; *********************************************************
; sprite_cram is the routine to update all sprites when   *
; an NMI is triggered.                                    *
; *********************************************************

sprite_cram:
	lda #>sprite
	sta $4014
	rts

nmi:
	pha
	txa
	pha
	tya
	pha
	jsr sprite_cram
	lda whose_turn
	cmp #$00
	beq skip_controls
	jsr controller
	lda cant_see_hidden
	cmp #$00
	bne skip_controls
	jsr play2
skip_controls:
	lda #$00
	sta $2006
	sta $2006
	sta $2005
	sta $2005
	jsr play
end_nmi:
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	rti

palette:
.incbin "ttxo.pal"

; *********************************************************
; The .bytes below are setup for the sprites to be used   *
; .byte (Y-Pos),(Tile Number),(Attributes),(X-Pos)        *
; *********************************************************
the_sprites:
	.byte $0d,$11,$00,$38		; Left side of arrow
	.byte $0d,$12,$00,$40		; Right side of arrow
	.byte $c0,$13,$00,$30		; Item Selection Arrow
	.byte $cd,$2f,$01,$38		; 0 - For Extra Turn
	.byte $fe,$30,$01,$fe		; 1 - For Extra Turn
	.byte $cd,$2f,$01,$68		; 0 - For Change Tile
	.byte $fe,$30,$01,$fe		; 1 - For Change Tile
	.byte $cd,$2f,$01,$98		; 0 - For Erase Row
	.byte $fe,$30,$01,$fe		; 1 - For Erase Row
	.byte $cd,$2f,$01,$c8		; 0 - For Erase Column
	.byte $fe,$30,$01,$fe		; 1 - For Erase Column
	
; *********************************************************
; Store the address of the background areas to be used    *
; by the X's and O's to be placed on the game board       *
; *********************************************************

filltopleft:
	.byte $66,$67,$68,$69
	.byte $86,$87,$88,$89
	.byte $a6,$a7,$a8,$a9
	.byte $c6,$c7,$c8,$c9
filltop:
    .byte $6e,$6f,$70,$71
    .byte $8e,$8f,$90,$91
    .byte $ae,$af,$b0,$b1
    .byte $ce,$cf,$d0,$d1
filltopright:
    .byte $76,$77,$78,$79
    .byte $96,$97,$98,$99
    .byte $b6,$b7,$b8,$b9
    .byte $d6,$d7,$d8,$d9
fillleft:
    .byte $66,$67,$68,$69
    .byte $86,$87,$88,$89
    .byte $a6,$a7,$a8,$a9
    .byte $c6,$c7,$c8,$c9
fillmiddle:
    .byte $6e,$6f,$70,$71
    .byte $8e,$8f,$90,$91
    .byte $ae,$af,$b0,$b1
    .byte $ce,$cf,$d0,$d1
fillright:
    .byte $76,$77,$78,$79
    .byte $96,$97,$98,$99
    .byte $b6,$b7,$b8,$b9
    .byte $d6,$d7,$d8,$d9
fillbottomleft:
    .byte $66,$67,$68,$69
    .byte $86,$87,$88,$89
    .byte $a6,$a7,$a8,$a9
    .byte $c6,$c7,$c8,$c9
fillbottom:
    .byte $6e,$6f,$70,$71
    .byte $8e,$8f,$90,$91
    .byte $ae,$af,$b0,$b1
    .byte $ce,$cf,$d0,$d1
fillbottomright:
    .byte $76,$77,$78,$79
    .byte $96,$97,$98,$99
    .byte $b6,$b7,$b8,$b9
    .byte $d6,$d7,$d8,$d9
clear_them:
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00
	.byte $00,$00,$00,$00

; *********************************************************
; Tiles to use for X and O                                *
; *********************************************************

x_tiles:
	.byte $80,$81,$00,$83
	.byte $00,$91,$92,$93
	.byte $00,$a1,$a2,$a3
	.byte $b0,$b1,$00,$b3

o_tiles:
	.byte $84,$85,$86,$87
	.byte $94,$00,$00,$97
	.byte $a4,$00,$00,$a7
	.byte $b4,$b5,$b6,$b7

; *********************************************************
; Include all of the nametables for the game below        *
; *********************************************************

title:
.incbin "ttxo_title.nam"

board:
.incbin "ttxo_board.nam"

instr0:
.incbin "ttxo_instr0.nam"

instr1:
.incbin "ttxo_instr1.nam"

beat_it:
.incbin "ttxo_final.nam"

thanks:
.incbin "ttxo_hidden.nam"

; *********************************************************
; Sound effects down here                                 *
; *********************************************************

sound_fx:
	.byte $c0,$50,$00,$1b		; Move main cursor sound
	.byte $c0,$00,$00,$0b		; Move item cursor sound

stalemate:
	.byte $52,$53,$40,$4b,$44,$4c,$40,$53,$44,$20
blank_under_stale:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
fix_message:
	.byte $09,$0a,$08,$08,$08,$08,$08,$08,$09,$0a
under_fix_message:
	.byte $0b,$0c,$07,$07,$07,$07,$07,$07,$0b,$0c
youwin:
	.byte $00,$58,$4e,$54,$00,$56,$48,$4d,$20,$00
game_over:
	.byte $46,$40,$4c,$44
	.byte $00,$00,$00,$00
	.byte $4e,$55,$44,$51
	.byte $00,$00,$00,$00
congratulations:
	.byte $42,$4e,$4d,$46,$51,$40,$53,$54,$4b,$40,$53,$48,$4e,$4d,$52,$20
press_start_words:
	.byte $4f,$51,$44,$52,$52,$00,$52,$53,$40,$51,$53,$20

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
