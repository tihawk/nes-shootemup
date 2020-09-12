.segment "HEADER"

    .byte "NES"
    .byte $1a
    .byte $02       ; 4 - 2*16k PRG ROM
    .byte $01       ; 5 - 8k CHR ROM
    .byte %00000001 ; 6 - mapper - horizontal mirroring <- refers to how graphics are organised (horizontal or vertical mirroring, depending on how the game scrolls) four-screen mirroring?
    .byte $00       ; 7 - 
    .byte $00       ; 8 - 
    .byte $00       ; 9 - NTSC
    .byte $00
    .byte $00, $00, $00, $00, $00 ; Filler

.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Bullet = 2
    FlyBy = 3
.endscope

.struct Entity
    xpos .byte
    ypos .byte
    type .byte
.endstruct

.segment "STARTUP"

.segment "ZEROPAGE"
; immediate vs zeropage mode of addressing memory (they have different timings) zeropage is in the first 255 bytes of memory
; 0x00 - 0xff
controller: .res 1  ; stores state of controller
scrollx:    .res 1  ; screen scroll
scrolly:    .res 1

MAXENTITIES = 10
entities:   .res .sizeof(Entity) * MAXENTITIES
TOTALENTITIES = .sizeof(Entity) * MAXENTITIES

buttonflag:     .res 1
swap:           .res 1  ; related to bg scrolling. scroll register allows for 255 values. this keeps memory of which offset is in use
hswaph:         .res 1
bgloadlo:       .res 1
bgloadhi:       .res 1
bglow:          .res 1
bghi:           .res 1
seed:           .res 2  ; initialise 16-bit seed to any value except 0. randomiser
flicker:        .res 1  ; counter for determining when the colour of the ship should flicker
spritemem:      .res 2
drawcomplete:   .res 1

.segment "CODE"

prng: 
    ; random number generator
    ldx #8      ; iteration count (generates 8 bits)
    lda seed+0
:
    asl         ; shift the register
    rol seed+1
    bcc :+
    eor #$2D    ; apply XOR feedback whenever a 1 bit is shifted out
:
    dex
    bne :--
    sta seed+0
    cmp #0      ; reload flags
    rts

WAITFORVBLANK:
    BIT $2002
    BPL WAITFORVBLANK
    RTS

RESET: ; boilerplate
    SEI         ; turn off interrupts
    CLD         ; turn off decimal maths
    LDX #$40    ; turn off audio interrupts
    STX $4017   ; -||-
    LDX #$FF    ; initialising prg stack
    TXS         ; -||-
    INX
    STX $2000   ; turn off PPU and audio rendering
    STX $2001
    STX $4010

    JSR WAITFORVBLANK

    TXA
CLEARMEM:
    STA $0000, x
    STA $0100, x
    STA $0300, x
    STA $0400, x
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FF
    STA $0200, x
    LDA #$00
    STA controller
    INX
    BNE CLEARMEM

    LDA #$21
    STA hswaph

; initialise entities+Entity::xpos
    LDA #$80
    STA entities+Entity::xpos
    LDA #$78
    STA entities+Entity::ypos
    LDA #EntityType::PlayerType
    STA entities+Entity::type

    LDA #$10
    STA $07F0

    LDX #$03
    LDA #$FF
CLEARENTITIES:
    LDA #$11
    STA $07F1
    STA entities+Entity::xpos, x
    STA entities+Entity::ypos, x
    LDA #$00
    STA entities+Entity::type, x
    LDA #$FF
    INX     ; faster to do 3 increments than an add because it's 2 less cycles
    INX
    INX
    CPX #TOTALENTITIES
    BNE CLEARENTITIES

    LDA #$12
    STA $07F2
; clear register and set palette address
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$10
    STA $2006

; initialise background hi and low
    LDA #$10
    STA seed
    STA seed+1

    LDA #$02
    STA scrolly

    LDX #$00
PALETTELOAD:
    LDA PALETTE, x
    STA $2007
    INX
    CPX #$20
    BNE PALETTELOAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA #$C0
    STA bgloadlo
    LDA #$03
    STA bgloadhi
    LDY #$00

    LDA $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006
BGLOAD:
    JSR prng
    LSR
    STA $2007
    INY
    CPY #$00
    BNE SKIPGBINC
    INC bghi
SKIPGBINC:
    DEC bgloadlo
    LDA bgloadlo
    CMP #$FF
    BNE BGLOAD
    DEC bgloadhi
    LDA bgloadhi
    CMP #$FF
    BNE BGLOAD

; configure for loading the attributes
    LDA $2002
    LDA #$23
    STA $2006
    LDA #$C0
    STA $2006
    LDX #$00
    TXA
ATTLOAD:
    STA $2007
    INX
    CPX #$08
    BNE ATTLOAD

    JSR WAITFORVBLANK

    LDA #%10000000
    STA $2000
    LDA #%00011110
    STA $2001

    ; load spritemem
    LDA #$00
    STA spritemem
    LDA #$02
    STA spritemem+1

GAMELOOP:
INITIALISESPRITES:
    LDY #$00
    LDA #$FF
INITIALISESPRITESLOOP:
    STA (spritemem), y
    INY
    EOR #$FF
    STA (spritemem), y
    INY
    STA (spritemem), y
    INY
    EOR #$FF
    STA (spritemem), y
    INY 
    BEQ startreadcontrollers
    JMP INITIALISESPRITESLOOP

startreadcontrollers:

    ; read controls
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readcontrollerbuttons:

    LDA $4016       ; a
    ROR A
    ROL controller
    LDA $4016       ; b
    ROR A
    ROL controller
    LDA $4016       ; select
    ROR A
    ROL controller
    LDA $4016       ; start
    ROR A
    ROL controller
    LDA $4016       ; up
    ROR A
    ROL controller
    LDA $4016       ; down
    ROR A
    ROL controller
    LDA $4016       ; left
    ROR A
    ROL controller
    LDA $4016       ; right
    ROR A
    ROL controller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkleft:
    LDA controller
    AND #$02
    BEQ checkright
    DEC entities+Entity::xpos
    JMP checkup ; dont allow for left and right at the same time

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    INC entities+Entity::xpos

checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    DEC entities+Entity::ypos
    JMP donecheckingdirectional ; dont allow for up and down to be pressed at the same time

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    INC entities+Entity::ypos

donecheckingdirectional:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkbuttons:

checka:
    LDA controller
    AND #$80
    BEQ checkarelease
    LDA buttonflag
    ORA #$01
    STA buttonflag
    JMP finishcontrols
checkarelease:
    LDA buttonflag
    AND #$01
    BEQ finishcontrols
    DEC buttonflag      ; for a this works because it's bit 1
    JMP addbullet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
addbullet:
    LDX #$00
addbulletloop:
    CPX #TOTALENTITIES
    BEQ finishcontrols                  ; if we've hit the max, then there are no more available entities for this
    LDA entities+Entity::type, x        ; get the entity type from memory
    CMP #EntityType::NoEntity           ; if this is a used entity slot?
    BEQ addbulletentity                 ; if it's free, add the bullet
    TXA                                 ; otherwise, increment to the next entity and loop
    CLC
    ADC #.sizeof(Entity)
    TAX
    JMP addbulletloop
addbulletentity:
    LDA entities+Entity::xpos           ; get player position and then offset bullet accordingly
    CLC
    ADC #$04
    STA entities+Entity::xpos, x
    LDA entities+Entity::ypos
    STA entities+Entity::ypos, x
    LDA #EntityType::Bullet
    STA entities+Entity::type, x
    JMP finishcontrols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finishcontrols:
processscrolling:
    LDA scrolly
    SEC
    SBC #$02
    STA scrolly
    CMP #$00
    BNE donescroll
    LDA #$EE
    STA scrolly
    LDA swap
    EOR #$02
    STA swap
donescroll:

processentities:
    LDX #.sizeof(Entity)
processentitiesloop:
    LDA entities+Entity::type, x
    CMP #EntityType::Bullet
    BNE skipentity
processbullet:
    LDA entities+Entity::ypos, x
    SEC
    SBC #$03
    STA entities+Entity::ypos, x
    BCS entitycomplete
    LDA #EntityType::NoEntity
    STA entities+Entity::type, x
    LDA #$FF
    STA entities+Entity::xpos, x
    STA entities+Entity::ypos, x
entitycomplete:
skipentity:
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    CMP #$1E
    BNE processentitiesloop
; doneprocessingentities:

hasdrawcompleted:
    LDA drawcomplete
    CMP #$01
    BNE hasdrawcompleted
    LDA #$00
    STA drawcomplete
    JMP GAMELOOP

VBLANK:

    PHA
    PHP
    TXA
    PHA
    TYA
    PHA

; begin populating the OAM data in memory
    LDX #$00
    LDA #$00
    LDY #$00
    STA spritemem
    LDA #$02
    STA spritemem+1
DRAWENTITIES:
    LDA entities+Entity::type, X
    CMP #EntityType::PlayerType
    BEQ PLAYERSPRITE
    CMP #EntityType::Bullet
    BEQ BULLET
    JMP CHECKENDPSRITE

BULLET:
    LDA entities+Entity::ypos, x ; y
    STA (spritemem), y
    INY
    LDA #$01 ; tile
    STA (spritemem), y
    INY
    LDA #$02 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    STA (spritemem), y
    INY
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    JMP CHECKENDPSRITE

FLYBY:
PLAYERSPRITE:
    ; top left sprite
    LDA entities+Entity::ypos, x ; y
    STA (spritemem), y
    INY
    LDA #$00 ; tile
    STA (spritemem), y
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    STA (spritemem), y
    INY

    ; bottom left sprite
    LDA entities+Entity::ypos, x ; y
    CLC
    ADC #$08 ; add 8 px to ypos
    STA (spritemem), y
    INY
    LDA #$10 ; tile
    STA (spritemem), y
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    STA (spritemem), y
    INY

    ; top right sprite
    LDA entities+Entity::ypos, x ; y
    STA (spritemem), y
    INY
    LDA #$00 ; tile
    STA (spritemem), y
    INY
    LDA #$41 ; palette and horizontal flip
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    CLC
    ADC #$08 ; add 8px to xpos
    STA (spritemem), y
    INY

    ; bottom right sprite
    LDA entities+Entity::ypos, x ; y
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    LDA #$10 ; tile
    STA (spritemem), y
    INY
    LDA #$41 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKENDPSRITE:
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    CPX #TOTALENTITIES
    BEQ DONESPRITE
    JMP DRAWENTITIES

DONESPRITE:
    INC flicker
    LDA flicker
    AND #$0C
    BNE noflicker

    INC hswaph
    LDA hswaph
    CMP #$23
    BNE skiproll
    LDA #$21
    STA hswaph

skiproll:
    ; clear register and set palette address
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$17
    STA $2006

    LDA hswaph
    STA $2007

noflicker:
    ; direct memory access copy sprites DMA load (copy sprites to PPU)
    LDA #$00
    STA $2003 ; reset counter
    LDA #$02    ; set memory to $0200 range
    STA $4014
    NOP         ; improve scan sync

    LDA #$00    ; clear out register
    STA $2006
    STA $2006

    LDA scrollx
    STA $2005
    LDA scrolly
    STA $2005

    LDA #%10001000
    ORA swap
    LDX $2002   ; clear the register before resetting because we're in the vblank
    STA $2000

donewithppu:
    PLA
    TAY
    PLA
    TAX
    PLP
    PLA
    inc drawcomplete
    RTI

PALETTE:
    .byte $0d, $30, $16, $27
    .byte $0d, $00, $10, $12
    .byte $0d, $0c, $1c, $3c
    .byte $0d, $00, $10, $12
    .byte $0d, $00, $10, $12
    .byte $0d, $00, $10, $12
    .byte $0d, $00, $10, $12
    .byte $0d, $00, $10, $12

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "shooter.chr"