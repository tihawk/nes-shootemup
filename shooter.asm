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
    Player   = 1
    Bullet   = 2
    FlyBy    = 3
.endscope

.scope GameState
    LoadTitleScreen = 0
    TitleScreen     = 1
    LoadNewGame     = 2
    GamePlay        = 3
    Paused          = 4
.endscope

; FlyBy entity self byte map
; ---- ----
; |||| ||||
; |||| |||+ Speed of flyby
; |||| ||+- null
; |||| |+-- null
; |||| +--- null
; |||+----- null
; ||+------ null
; |+------- null
; +-------- null

.struct Entity
    xpos .byte
    ypos .byte
    xvel .byte
    yvel .byte
    type .byte
    self .byte ; entity-specific data
.endstruct

.segment "STARTUP"

.segment "ZEROPAGE"
; immediate vs zeropage mode of addressing memory (they have different timings) zeropage is in the first 255 bytes of memory
; 0x00 - 0xff
controller: .res 1  ; stores state of controller
scrollx:    .res 1  ; screen scroll
scrolly:    .res 1

MAXENTITIES = 20
entities:   .res .sizeof(Entity) * MAXENTITIES
TOTALENTITIES = .sizeof(Entity) * MAXENTITIES
POSITIVE_EXTREMUM_VELOCITY = 15
NEGATIVE_EXTREMUM_VELOCITY = $F1

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
collisiontmp:   .res 1  ; stores variables for collision detection subroutine
boundingleft:   .res 1
boundingright:  .res 1
boundingtop:    .res 1
boundingbottom: .res 1
gamestate:      .res 1

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
    LDA #$00
    STA entities+Entity::xvel
    STA entities+Entity::yvel
    LDA #EntityType::Player
    STA entities+Entity::type

    LDA #$10
    STA $07F0

    LDX #.sizeof(Entity)
    LDA #$FF
CLEARENTITIES:
    STA entities+Entity::xpos, x
    STA entities+Entity::ypos, x
    LDA #$00
    STA entities+Entity::type, x
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    LDA #$FF
    CPX #TOTALENTITIES
    BNE CLEARENTITIES

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
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP



    LDA gamestate
    CMP #GameState::LoadTitleScreen
    BEQ LOAD_TITLE_SCREEN_STATE
    CMP #GameState::TitleScreen
    BEQ TITLE_SCREEN_STATE
    CMP #GameState::LoadNewGame
    BEQ LOAD_NEW_GAME_STATE
    CMP #GameState::GamePlay
    BEQ GAMEPLAY_STATE
    CMP #GameState::Paused
    BEQ PAUSED_STATE
; PROBLEM:
;     JMP PROBLEM

LOAD_TITLE_SCREEN_STATE:
    ; load titlescreen assets to name table
    LDA #GameState::TitleScreen
    STA gamestate
    JMP GAMELOOP

TITLE_SCREEN_STATE:
    LDA controller
    AND #$10
    BEQ GAMELOOP
    LDA #GameState::LoadNewGame
    STA gamestate
    JMP hasdrawcompleted

LOAD_NEW_GAME_STATE:
    ; load game assets to name table
    ; initialise other stuff
    LDA #GameState::GamePlay
    STA gamestate
    JMP GAMELOOP

PAUSED_STATE:
    LDA controller
    AND #$10
    BEQ GAMELOOP
    LDA #GameState::GamePlay
    STA gamestate
    JMP hasdrawcompleted

GAMEPLAY_STATE:
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
    BEQ spriteinitfinished
    JMP INITIALISESPRITESLOOP

spriteinitfinished:

checkleft:
    LDA controller
    AND #$02
    BEQ checkright
    DEC entities+Entity::xvel
    DEC entities+Entity::xvel
    JMP checkup ; dont allow for left and right at the same time

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    INC entities+Entity::xvel
    INC entities+Entity::xvel

checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    DEC entities+Entity::yvel
    DEC entities+Entity::yvel
    JMP donecheckingdirectional ; dont allow for up and down to be pressed at the same time

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    INC entities+Entity::yvel
    INC entities+Entity::yvel

donecheckingdirectional:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkbuttons:

checkstart:
    LDA controller
    AND #$10
    BEQ checkstartrelease
    LDA buttonflag
    ORA #$04
    STA buttonflag
    JMP checkb
checkstartrelease:
    LDA buttonflag
    AND #$04
    BEQ checkb
    LDA buttonflag
    EOR #$04
    STA buttonflag
    LDA GameState::Paused
    STA gamestate
    JMP hasdrawcompleted

checkb:
    LDA controller
    AND #$40
    BEQ checkbrelease
    LDA buttonflag
    ORA #$02
    STA buttonflag
    JMP checka
checkbrelease:
    LDA buttonflag
    AND #$02
    BEQ checka
    LDA buttonflag
    EOR #$02
    STA buttonflag
    JMP addflyby

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
    LDA buttonflag
    EOR #$01
    STA buttonflag
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
addflyby:
    LDX #$00
addflybyloop:
    CPX #TOTALENTITIES
    BEQ finishcontrols
    LDA entities+Entity::type, x
    CMP #EntityType::NoEntity
    BEQ addflybyentity
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    JMP addflybyloop
addflybyentity:
    LDA entities+Entity::xpos
    STA entities+Entity::xpos, x
    LDA #$08
    STA entities+Entity::ypos, x
    LDA #EntityType::FlyBy
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
    LDX #$00
processentitiesloop:
    LDA entities+Entity::type, x
    CMP #EntityType::Player
    BEQ processplayer
    CMP #EntityType::Bullet
    BNE notbullet_skiptonexttype
    JMP processbullet
notbullet_skiptonexttype:
    CMP #EntityType::FlyBy
    BNE notflyby_skiptonexttype
    JMP processflyby
notflyby_skiptonexttype:
    JMP skipentity
processplayer:
    LDA entities+Entity::xvel, x
    BEQ processplayerxveldone           ; if the velocity is zero, we're done
    BPL processplayerremovexvel         ; if velocity is positive, go to decrement
    SEC
    SBC #NEGATIVE_EXTREMUM_VELOCITY
    BVS skipxveleor
    EOR #$80
skipxveleor:
    BPL clampnegativexvel
    LDA entities+Entity::xvel, x
    JMP skiplowerboundingxvel
clampnegativexvel:
    LDA #NEGATIVE_EXTREMUM_VELOCITY
    STA entities+Entity::xvel, x
skiplowerboundingxvel:
    SEC                                 ; velocity is negative, so we first set the carry
    ROR                                 ; to be able to divide by 4
    SEC
    ROR
    CLC
    ADC entities+Entity::xpos, x        ; and then add that to the xpos of the player
    STA entities+Entity::xpos, x
    INC entities+Entity::xvel, x        ; negative velocity, increment
    JMP processplayerxveldone
processplayerremovexvel:
    CMP #POSITIVE_EXTREMUM_VELOCITY                            ; max velicity is 40, so compare and limit accordingly
    BEQ skipupperboundingxvel
    BCC skipupperboundingxvel
    LDA #POSITIVE_EXTREMUM_VELOCITY
    STA entities+Entity::xvel, x
skipupperboundingxvel:
    CLC                                 ; velocity is positive, so we first clear the carry
    ROR                                 ; to be able to divide by 4
    CLC
    ROR
    CLC
    ADC entities+Entity::xpos, x        ; and then add that to the xpos of the player
    STA entities+Entity::xpos, x
    DEC entities+Entity::xvel, x        ; positive velocity, decrement
    JMP processplayerxveldone
processplayerxveldone:
    LDA entities+Entity::yvel, x
    BEQ entitycomplete                  ; if the velocity is zero, we're done
    BPL processplayerremoveyvel         ; if velocity is positive, go to decrement
    SEC
    SBC #NEGATIVE_EXTREMUM_VELOCITY
    BVS skipyveleor
    EOR #$80
skipyveleor:
    BPL clampnegativeyvel
    LDA entities+Entity::yvel, x
    JMP skiplowerboundingyvel
clampnegativeyvel:
    LDA #NEGATIVE_EXTREMUM_VELOCITY
    STA entities+Entity::yvel, x
skiplowerboundingyvel:
    SEC                                 ; velocity is negative, so we first set the carry
    ROR                                 ; to be able to divide by 4
    SEC
    ROR
    CLC
    ADC entities+Entity::ypos, x        ; and then add that to the xpos of the player
    STA entities+Entity::ypos, x
    INC entities+Entity::yvel, x        ; negative velocity, increment
    JMP entitycomplete
processplayerremoveyvel:
    CMP #POSITIVE_EXTREMUM_VELOCITY                            ; max velicity is 40, so compare and limit accordingly
    BEQ skipupperboundingyvel
    BCC skipupperboundingyvel
    LDA #POSITIVE_EXTREMUM_VELOCITY
    STA entities+Entity::yvel, x
skipupperboundingyvel:
    CLC                                 ; velocity is positive, so we first clear the carry
    ROR                                 ; to be able to divide by 4
    CLC
    ROR
    CLC
    ADC entities+Entity::ypos, x        ; and then add that to the xpos of the player
    STA entities+Entity::ypos, x
    DEC entities+Entity::yvel, x        ; positive velocity, decrement
    JMP entitycomplete
processbullet:
    LDA entities+Entity::ypos, x
    SEC
    SBC #$02
    ; check collision with other entities
    ; if collision occurs, destroy the fliby and bullet
    JSR checkbulletcollision
    STA entities+Entity::ypos, x
    BCS entitycomplete              ; if the carry was cleared after subtraction, the ypos has flipped on the other side. remove
    JMP clearentity
processflyby:
    ; todo, change how speed is handled, it currently uses the whole SELF byte
    INC entities+Entity::self, x
    LDA entities+Entity::self, x
    CMP #$02
    BNE skipmove
    LDA #$00
    STA entities+Entity::self, x
    INC entities+Entity::ypos, x
skipmove:
    LDA entities+Entity::ypos, x
    CMP #$FE
    BNE entitycomplete             ; if the carry was set after addition, the ypos has flipped. remove
    JMP clearentity
clearentity:
    LDA #EntityType::NoEntity
    STA entities+Entity::type, x
    LDA #$FF
    STA entities+Entity::xpos, x
    STA entities+Entity::ypos, x
    JMP entitycomplete
entitycomplete:
skipentity:
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    CMP #TOTALENTITIES
    BEQ doneprocessingentities
    JMP processentitiesloop
doneprocessingentities:

hasdrawcompleted:
    LDA drawcomplete
    CMP #$01
    BNE hasdrawcompleted
    LDA #$00
    STA drawcomplete
    JMP GAMELOOP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  checkbulletcollision
;;  in: X contains index of current entity (e.g. bullet), out: none, note: modifies Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkbulletcollision:
    PHA     ; A, X, P
    TXA
    PHA
    TAY     ; this contains the index of the entity from preceeding call
    PHP
    LDX #.sizeof(Entity)
checkbulletcollisionloop:
    CPX #TOTALENTITIES
    BEQ finishedbulletcollision
    LDA entities+Entity::type, x
    CMP #EntityType::FlyBy
    BNE checkbulletcollisionentityfinished
    
    ; precalculate bounding box
    LDA entities+Entity::xpos, x
    STA boundingleft
    CLC
    ADC #$08
    STA boundingright
    LDA entities+Entity::ypos, x
    STA boundingtop
    CLC
    ADC #$08
    STA boundingbottom

    TXA
    PHA
    LDA BULLETCOLLISIONPOINTSCOUNT
    STA collisiontmp
    LDX #$00
collisionpointsloop:
    CPX collisiontmp
    BEQ collisionpointsloopfailed

    LDA entities+Entity::xpos, y    ; get bullet's current xpos bul_x
    CLC
    ADC BULLETCOLLISIONPOINTSX, x   ; get left-most bullet point bul_x + 1
    CMP boundingleft                ; compare to flyby xpos fly_x. we care if bul_x+1 >= fly_x
    BCC checkcollisionpointnext     ; CMP sets the carry if A >=, so if not set bul_x+1 < fly_x
    CMP boundingright                ; compare to see if bul_x+1 <= fly_x+8
    BEQ skipx
    BCS checkcollisionpointnext     ; if bul_x+1 > fly_x+8, no collision
skipx:
    ; we now know we're within x-range, now let's check the ypos range
    LDA entities+Entity::ypos, y    ; bul_y
    CLC
    ADC BULLETCOLLISIONPOINTSY, x
    CMP boundingtop                 ; conmpare to see if bul_y+4 >= fly_y
    BCC checkcollisionpointnext
    CMP boundingbottom              ; compare to see if bul_y+4 <= fly_y+8
    BEQ skipy
    BCS checkcollisionpointnext     ; if carry set, then bul_y+4 >= fly_y+8
skipy:
    JMP collisionpointsloopfinished
checkcollisionpointnext:
    INX
    JMP collisionpointsloop
collisionpointsloopfinished:
    PLA
    TAX
    ; we now know that both xpos and ypos of the left-most side of the bullet are within the bounds of the flyby
    LDA #EntityType::NoEntity
    STA entities+Entity::type, y    ; destory bullet
    STA entities+Entity::type, x    ; destroy entity
    LDA #$FF
    STA entities+Entity::xpos, x
    STA entities+Entity::ypos, x
    STA entities+Entity::xpos, y
    STA entities+Entity::ypos, y
    JMP finishedbulletcollision
collisionpointsloopfailed:
    PLA
    TAX
checkbulletcollisionentityfinished:
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    JMP checkbulletcollisionloop

finishedbulletcollision:
    PLP     ; P, X, A
    PLA
    TAX
    PLA
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    CMP #EntityType::Player
    BEQ PLAYERSPRITE
    CMP #EntityType::Bullet
    BEQ BULLET
    CMP #EntityType::FlyBy
    BEQ FLYBY
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
    LDA entities+Entity::ypos, x ; y
    STA (spritemem), y
    INY
    LDA #$02 ; tile
    STA (spritemem), y
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x
    STA (spritemem), y
    INY
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    JMP CHECKENDPSRITE
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
    INC drawcomplete
    RTI

BULLETCOLLISIONPOINTSCOUNT:
    .byte $03
BULLETCOLLISIONPOINTSX:
    .byte $01, $04, $07
BULLETCOLLISIONPOINTSY:
    .byte $04, $01, $04

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