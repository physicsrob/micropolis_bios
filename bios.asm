;---------------------------------------------------------------
;
;	CP/M 2.2 BIOS for Micropolis 
;
;	Disassembled by Robert Porter in November,2022
;
;	References:
;	Original code was extracted from CP/M Lifeboat 2.2
;		for Micropolis disk image, located at
;		https://deramp.com/downloads/vector_graphic/software/disk_images/Micropolis%20controller/56K%20Bootable%20Disk%20Images.zip
;
;	Micropolis Driver
;		https://deramp.com/downloads/vector_graphic/software/manuals/Disk%20Driver.pdf
;		The code for the micropolis driver was
;		incredibly helpful.  Some of the disk logic was
;		exactly the same as in the driver.  Where
;		possible I copied routine names and comments.
;		In other cases the logic here is quite modified.
;		For instance this driver code supports double
;		sided disks.
;
;	CP/M Operating System Manual
;		Section 6,in general, but especially Appendix G.
;		The read/write blocking and deblocking seems to
;		be heavily inspired by the manual example.
;
;	Physical disk layout
;		Length	Content
;		   1	Sync byte (always 0xFF)
;		   1	Track number (0-76 for mod-II,
;				      0-35 for mod-I)
;		   1	Sector number (0-15)
;		  10	Unused
;		 128	First CP/M logical sector payload
;		 128	Second CP/M logical sector payload
;		   1	Checksum
;
;	To patch changes made to this BIOS into a CP/M image
;	saved from MOVCPM (e.g., CPMxx.COM), use following
;	commands:
;
;	A>DDT CPMxx.COM
;	-IBIOS.HEX
;	-Rxxxx		(where xxxx = BIAS computed below)
;	-G0		(Go zero, not "oh")
;	A>SYSGEN
;---------------------------------------------------------------

; CP/M size and location values

memsize equ	40		;memory size
biosLen equ	0C00h		;length of the BIOS
ccpLen	equ	0800h		;CPM 2.2 fixed length
bdosLen	equ	0e00h		;CPM 2.2 fixed length
sysgen	equ	0900h		;sysgen image location

; These equates are automatically changed by memSize and biosLen

biosBas	equ	memSize*1024-biosLen	;base addr of the BIOS
bdosBas	equ	biosBas-bdosLen		;base addr of the BDOS
ccpBase	equ	bdosBas-ccpLen		;base addr of the CCP
bdosEnt	equ	bdosBas+6		;BDOS entry address

; User Area
usrLen	equ	0200h	;512 bytes for USER area
usrOff	equ	0600h	;offset of USER area in BIOS
usrBase	equ	memsize*1024 - biosLen + usrOff

bootLen	equ	256			;boot loader length
bias	equ	sysgen+bootLen-ccpBase	;bias in DDT to patch

; Address of buffer/parameter area above user area
prmBase	equ	usrBase + usrLen

; Stack Area
stack	equ	memsize*1024 - 80H;

; CPM page zero equates

wbootv	equ	00h		;warm boot vector location
bdosv	equ	05h		;bdos entry vector location
cdisk	equ	04h		;CPM current disk
defDMA	equ	80h		;default dma address


; External Addresses
bdos	equ	bdosBas + 6H
ccpInit	equ	ccpBase + 3H


; USER Area Addresses
ucoldst	equ	usrBase
uwarmst	equ	usrBase+3H
uconst	equ	usrBase+6H
uconin	equ	usrBase+9H
uconout	equ	usrBase+0CH
uList	equ	usrBase+0FH
uPunch	equ	usrBase+12H
uReader	equ	usrBase+15H
uListst	equ	usrBase+18H

SPT	equ	40h		;logical sectors per track
				;updated on boot
BSH	equ	4h		;data allocation block shift
BLM	equ	0Fh		;data allocation block mask
EXM	equ	0h		;extent mask
DSM	equ	12bh		;determines total storage cap
				;updated on boot
DRM	equ	7Fh		;det. total num of dir entries
AL0	equ	0C0h		;det. reserved directory blocks
AL1	equ	0h		;ditto
CKS	equ	10h		;size of dir check vectors
OFF	equ	2h		;number of reserved tracks at
				;beginning of the logical disk

unkTrk	equ	0FFh		;represents unknown track pos
syncByt	equ	0FFh		;sync byte at start of sector
secLen	equ	270		;total physical sector length
cSecLen	equ	128		;CP/M sector length
dskOpRd	equ	1		;dskIO flag
dskOpWr	equ	3		;dskIO flag

;---------------------------------------------------------------
;
;	BDOS CONSTANTS ON ENTRY TO write
;		(VALUES FOR wrType)
;
;---------------------------------------------------------------
WRALL	equ	0		;write TO ALLOCATED
WRDIR	equ	1		;write TO DIRECTORY
WRUAL	equ	2		;write TO UNALLOCATED


;---------------------------------------------------------------
;	Micropolis Controller Register Definitions and Flags
;---------------------------------------------------------------
;	Sector register is at FDCBASE
;	0-3	SECTOR COUNT
;	 4	SPARE
;	 5	SPARE
;	 6	SCTR INTERRUPT FLAG
;	 7	SECTOR FLAG
;
;	FLAG BITS
SIFLG	equ	040H
SFLG	equ	080H
DTMR	equ	020H

;	Status register is at FDCBASE+1
;	0-1	UNIT ADDRESS
;	 2	UNIT SELECTED (LOW TRUE)
;	 3	TRACK 0
;	 4	WRITE PROTECT
;	 5	DISK READY
;	 6	PINTE
;	 7	TRANSFER FLAG
;
;	FLAG BITS

TFLG	equ	80H
INTE	equ	40H
RDY	equ	20H
WPT	equ	10H
TK0	equ	08H
USLT	equ	04H

; Command register at FDCBASE (and again at FDCBASE+1)
;	bits 0-4 COMMAND MODIFIER
;	bits 5-7 COMMAND

SLUN	equ	020H		;SELECT UNIT
;  MODIFIER = DRIVE ADDRESS PLUS
;             10H FOR UPPER HEAD SELECT (FOR DOUBLE SIDED DRV)

SINT	equ	040H		;SET INTERRUPT
;  MODIFIER = 1 ENABLE INTERRUPT
;             0 DISABLE INTERRUPT

STEP	equ	060H		;STEP CARRIAGE
;  MODIFIER = 00 STEP OUT
;             01 STEP IN

WTCMD	equ	080H		;ENABLE WRITE
;  NO MODIFIER USED

RESET	equ	0A0H		;RESET CONTROLLER
;  NO MODIFIER USED




	org	biosBas
;---------------------------------------------------------------
;
;  BIOS Entry Jump Table
;
;---------------------------------------------------------------

	jmp	boot
jwboot  jmp	wboot
	jmp	uconst
	jmp	uconin
	jmp	conOut
	jmp	uList
	jmp	uPunch
	jmp	uReader
	jmp	home
	jmp	selDsk
	jmp	setTrk
	jmp	setSec
	jmp	setDMA
	jmp	read
	jmp	write
	jmp	uListst
	jmp	secTran

;---------------------------------------------------------------
; boot - Cold boot BIOS entry. CPM is already loaded in memory.
;    Hand control to the CCP.
;---------------------------------------------------------------
boot:
	xra	a
	sta	cdisk		;set CP/M current disk to 0

	; Copy disk IO stack to 50H
	; This area is unused by CP/M,and it's unclear to me
	; why you would want to do this.
	lxi	h,stkPtr
	shld	50H

	call	rstFlgs
	call	ucoldst
	jmp	entCpm


;---------------------------------------------------------------
; wboot - Warm boot BIOS entry. Reload CPM from disk up to,but
;    not including the BIOS. Re-enter CPM after loading.
;---------------------------------------------------------------
wboot:
	lxi	sp,100H
	call	rstFlgs
	call	wboot0

;---------------------------------------------------------------
; entCpm - enter CPM. Set page zero variables,enter CPM with or
;    without command line based on the flags variable
;---------------------------------------------------------------
entCpm:
	; Initialize the first 8 bytes of memory
	; <JMP jwboot> < > < > <JMP BDOS>
	mvi	a,0C3H		;8080 JMP opcode (C3)
	sta	wbootv
	sta	bdosv

	lxi	h,jwboot	;store jwboot
	shld	wbootv + 1
	lxi	h,bdos		;store BDOS
	shld	bdosv + 1

	; Check cldDone flag
	; This is set to zero during boot (outside BIOS)
	lxi	h,cldDone
	mov	a,m
	ora	a
	; Z flag is set if cldDone hasn't been initialized
	mvi	m,1		;set cldDone to 1
	push	psw

	; Print startup message if cldDone is false
	; Which apparently is stored at the end of BDOS,right
	; before BIOS. Also it gets clobbered after load
	lxi	h,biosBas - 4CH
	cz	dispMsg

	; Restore Z flag
	pop	psw
	push	psw
	cnz	uwarmst		;call if cldDone was true

	lda	cdisk
	mov	c,a
	pop	psw

	lda	flags		;check flags
	rrc			;flags shifted right once
	jz	coldSt		;zero flag still set by cldDone
	rrc			;If cldDone,shift flags again
coldSt:
	; If cold start,carry is now bit 1 of flags, fCLDCMD
	; If not cold,carry is now bit 2 of flags, fWRMCMD
	; We jump depending on if we want to execute cmd line
	jnc	ccpInit		;no,ent CCP and clear cmd line
	jmp	ccpBase		;yes,ent CCP with possible cmd
	
	db	0CDH		;dead code

wboot0:
	; Called early in wboot.  Reload CPM from disk up to,
	; but not including the BIOS. Re-enter CPM after load.

	; Select disk 0
	mvi	c,0
	call	selDsk

	; Set DMA address to CCP base address
	lxi	h,ccpBase
	shld	dmaAddr

	; Read first track
	xra	a		;track (a) = 0
	mvi	l,7		;sector = 7
	mvi	h,30		;read 30 sectors
	call	loadTk		;read first track

	; Fall through to loadTk reading second track
	mvi	a,1		;track (a) = 1
	mvi	l,1		;sector = 1
	mvi	h,14		;read 14 sectors

;---------------------------------------------------------------
; loadTk -- load 1 track of data
;	Only used for loading system -- applies a different skew
;	table than the one used by CP/M.  
; On Entry:
;	a = track number (zero based)
;	l = first sector of track to read
;	h = number of tracks to read
;	dmaAddr = destination of load
;---------------------------------------------------------------
loadTk:
	sta	cpmTrk
ltk00:
	push	h		;save HL for later
	mov	a,l
	sta	cpmSec
	mvi	b,10		;read retry count
ltk05:
	push	b
	call	read
	pop	b
	jz	ltk10		;read successful?
	dcr	b
	jnz	ltk05		;retry
	jmp	wboot		;no more retries. wboot again.
ltk10:
	; Read was successful
	lhld	dmaAddr
	lxi	d,cSecLen
	dad	d
	shld	dmaAddr		;add 128 bytes to dmaAddr
	pop	h		;restore HL
	mov	a,l		;load current sector
	ani	1		;check if sector is odd
	jnz	ltk15
	mvi	a,5
ltk15:
	; The skew table applied here is not the same as the 
	; xlate table.  I guess there's no reason they have to
	; be the same.

	; We step through sectors incrementing 5, and then 1.
	; e.g. for the first call, with l=7, looks like:
	; 7, 8, 13, 14, 19, 20, 25, 26, 31, 32
	; 5, 6, 11, 12, 17, 18, 23, 24, 29, 30
	; 3, 4,  9, 10, 15, 16, 21, 22, 27, 28
	; The pairs of reads are from the same physical sector,
	; and we skip 2 physical sectors at a time.

	;if sector is odd, a=1, if sector is even, a=5
	;we effectively add this skew and wrap around at 32
	add	l
	dcr	a
	ani	01FH		;wrap around at 32
	inr	a
	mov	l,a
	dcr	h               ;decrement remaining sectors
	jnz	ltk00		;finished reading? loop
	ret			;done.


;---------------------------------------------------------------
; selDsk - Select the disc in register C (0-3)
;    Validate the disk number and return a pointer to the disk
;    parameter header in HL. Zero is returned in HL for invalid
;    drive number. The selected disk number is stored in
;    diskNum. No actual drive activity takes place.
;---------------------------------------------------------------
selDsk:
	lxi	h,selDsk0
	push	h

	; Strip top bit off C,and make sure it's (0-3)
	mov	a,c
	ani	07FH
	cpi	4
	rnc			;invalid disk (greater than 3)
	sta	cpmDrv		;store in cpmDrv
	call	upDPrm
	rnz
	pop	h
	lda	cpmDrv
	mov	l,a
	mvi	h,0
	dad	h
	dad	h
	dad	h
	dad	h
	lxi	d,dpHead
	dad	d
	ret

selDsk0:
	lxi	h,0
	xra	a
	sta	cdisk
	inr	a
	ret

home:
	mvi	c,0
setTrk:
	mov	a,c
	sta	cpmTrk
	ret
setSec:
	mov	a,c
	sta	cpmSec
	ret
setDMA:
	mov	h,b
	mov	l,c
	shld	dmaAddr
	ret

;---------------------------------------------------------------
; secTran - Sector translation BIOS entry. Convert logical
;	sector number in BC to physical sector number in HL
;	using the skew table passed in DE.
; Note: This does not account for the divide by two for blocking
;	and deblocking, only the skew table.
;---------------------------------------------------------------
secTran:
	xchg			;HL = skew table
	mvi	b,0		;c still contains logical sec
	dad	b		;HL = skew table + logical sec
	mov	l,m		;L = physical sector
	mov	h,b		;H = B = 0
	ret			;H=0,L=physical sector

dead0:
	; called by dead code
	db	021H

conOut:
	push	b
	; If we have unwritten sector,flush it to dsk
	call	flushWrt
	pop	b
	jmp	uconout

;---------------------------------------------------------------
; read - Read sector BIOS entry. Read one logical sector using 
;   the cpmTrk,cpmSec, and dmaAddr specified for diskNum.
; Returns   0 in A if successful,
;           1 in A if a non-recoverable error has occured.
;---------------------------------------------------------------
read:
	; If we have unwritten sector,flush it first
	call	flushWrt
	rnz			;fail the read if flush fails
	lda	cpmSec
	dcr	a
	cz	rstFlgs
	call	alloc	;unacnt=0,rsflag=1, a=1
	sta	readOp		;set readOp=1
	jmp	rwOper

;---------------------------------------------------------------
; write - Write sector BIOS entry. Write one sector to cpmDrv, 
;	cpmTrk, cpmSec, using dmaAddr. 
; returns
;	0 in A if successful,
;	1 in A if a non-recoverable error has occured.
;
; C=WRALL(0) - Write to allocated sector
; C=WRDIR(1) - Write to directory.  Write must be immediate
; C=WRUAL(2) - Write to unallocated sector.
;---------------------------------------------------------------
write:
	xra	a
	sta	readOp		;not a read operation
	call	wrt00		;most logic in wrt00
	jmp	rwOper

wrt00:
	; Store wrType for later
	mov	a,c
	sta	wrType
	ora	a		;wrType=WRALL?
	jz	wrt10		;yes, write to allocated sector
	
	cpi 	WRUAL		;wrType=WRUAL?
	mvi	a,10H
	jz	wrt05		;yes, write to unallocated sec
	xra	a

wrt05:
	; Case 1: WRDIR.  unacnt is set to 0
	; Case 2: WRUAL.  unacnt is set to 10h
	; unacnt is a bit weird, to be honest.  In the CPM
	; reference implementation, unacnt represents the number
	; of unallocated sectors left.
	; In this implementation it's used more as a flag.
	sta	unacnt

	lxi	h,cpmDrv
	lxi	d,wrtDrv
	call	memcpy3b	;copy cpmDrv,cpmTrk,cpmSec
				;  to wrtDrv,wrtTrk,wrtSec

wrt10:
	lxi	h,unacnt
	dcr	m
	jm alloc		;no unallocated sectors left

	; If we get here we're writing to an unallocated sector.
	; We batch the two 128-byte write calls to make a single
	; disk write (since we fit two 128-byte CPM sectors into
	; one disk sector).
	
	; Compare cpmDrv,cpmTrk, cpmSec
	; with wrtDrv,wrtTrk, wrtSec
	lxi	d,cpmDrv
	lxi	h,wrtDrv 
	call	cmpLoc

	; If they're different, set unacnt=0, rsflag=1
	; This is always true if wrType=WRALL
	jnz	alloc		;returns to write where rwOper
				;will happen next

	; Otherwise,they're the same
	; cmpLoc leaves HL/DE pointing to the 3rd byte (sector)

	; Check to see if the sector byte is exactly the same
	ldax	d		;a=cpmSec
	cmp	m
	jnz	alloc	;if they're not exactly the same
				;write the sector immediately.

	; Continuing along, we're still in the unallocated
	; sector case.  Meaning we're writing to a previously
	; empty sector on disk, and this call is either for the
	; first 128 bytes of the sector, or the second 128 bytes
	; of the sector.

	; We look up the next sector we will be writing to.
	; But note -- if we call write again with the second 128
	; bytes,we just override these values back to the
	; previous ones.

	; Loop through xlate table until we find cpmSec
	lxi	h,xlate
wrt15:
	cmp	m
	inx	h
	jnz	wrt15

	; HL is pointing at entry of xlate immediately following
	; the entry which matches cpmSec
	mov	a,m
	cpi	1		;last entry of xlate table?
	jnz	wrt20		;no, call wrt20
    
	; We're on the last entry apparently
	lxi	h,wrtTrk
	inr	m		;increment wrtTrk

wrt20:
	; a contains the next sector in the xlate table
	sta	wrtSec		;store the next sec in hostSec

	xra	a
	sta	rsflag		;set rsflag to 0
	ret			;ret to write (jmp to rwOper)

alloc:
	; not an unallocated record, requires preread,
	; or is an unallocated record that has been finished.
	; In the case of a finished unallocated record,
	; rsflag is ignored since hstact is true.
	xra	a		;0 to accum
	sta	unacnt		;unacnt=0
	inr	a
	sta	rsflag		;rsflag=1
	ret

;---------------------------------------------------------------
; rwOper - All disk I/O from read/write pass through this code
;   before being passed off to micropolis driver code in dskIO.
;
;  Flags that control behavior:
;	readOp - Are we currently performing a read?
;	rsflag - Do we need to do a read?
;	hstact - Is the buffer being used?
;	hstwrt - Is there a pending write?
;	wrType - write immediate if and only if WRDIR.
;
;  Responsible for copying from cpmDrv,cpmTrk,cpmSec to 
;	bufDrv,bufTrk,bufSec.
;  Responsible for copying data from/to dmaAddr and buf.
;---------------------------------------------------------------
rwOper:
	lxi	h,hstact	;host active flag
	mov	a,m 
	mvi	m,1		;always becomes 1
	ora	a		;was it already?
	jz	filhst		;fill host if not

	; Compare drive number,track number and sector
	; between cpmDrv,bufDrv
	lxi	h,bufDrv
	lxi	d,cpmDrv
	call	cmpLoc

	; If they're the same,we call match
	jz	match

	; not match. buffer drive, track, or sector different.
	lda	hstwrt		;host written?
	ora	a
	cnz	writeHst	;clear host buff
	rnz

filhst: 
	; May have to fill the host buffer
	; Sets bufDrv/Trk/Sec to cpmDrv/Trk/Sec, and reads into
	; buffer UNLESS rsflag is false, which is only in the
	; case of an unallocated write.

	xra	a
	sta	hstwrt		;reset hstwrt to False
	lxi	h,cpmDrv
	lxi	d,bufDrv
	call	memcpy3b	;copy cpmDrv,cpmTrk,cpmSec to
				; bufDrv,bufTrk, bufSec
	lda	rsflag
	ora	a

	; If rsflag is set, initiate read via dskIO.
	; rsflag is only off in the case of unallocated write.
	cnz	readHst
	rnz

match:
	; Match! cpmDrv/bufDrv triplet are the same,or
	; we just did filhst.

	lda	cpmSec
	rrc

	; Point DE to the first or second 128 byte chunk within
	; the buf based on the logical sector.
	lxi	d,bfData1
	jc	match0		;jump if sector is odd
	lxi	d,bfData2
match0:
	; DE=bfData1 if sector is odd
	; DE=bfData2 if sector is even
	lhld	dmaAddr		;HL=dmaAddr
	lda	readOp		;1 = read,0 = write
	ora	a
	jnz	match1		;skip below if read

	; Writing.  Swap direction of copy.
	mvi	a,1
	sta	hstwrt
	xchg
match1:
	xchg
	; If we're writing, we had two xchg, and reading only
	; one, so...
	; Read:  DE=dmaAddr, HL=bfData1/2
	; Write: DE=bfData1/2, HL=dmaAddr

	mvi	b,cSecLen
	call	memcpy		;copy 128 bytes from HL to DE

	lda	readOp
	dcr	a
	rz			;if we're reading,return here

	; Data has been moved to/from host buffer
	lda	wrType		;write type
	cpi	WRDIR		;to directory?
	jz	flushWrt	;yes,must write immediately
	xra	a
	ret

flushWrt:
	; Flush the current write buffer to disk
	lda	hstwrt
	ora	a
	rz			;do nothing if hstwrt is true
	call	rstFlgs	;otherwise reset flags
writeHst:
	; Just calls dskIO for write
	mvi	a,dskOpWr
	jmp	dskIO

	; Appears to be dead code
	call	dead0		
	db  21H
readHst:
	; Just calls dskIO for read
	mvi     a,dskOpRd
	jmp	dskIO

;---------------------------------------------------------------
; cmpLoc - Compares drive/track/sector triplets pointed to by HL
;	and DE
; Drive number must be the same.
; Track number must be the same.
; Physical sector must be the same
;	( FLOOR((Sec-1)/2) must be the same )
; Returns
;   non-zero if drv/trk/sec are different
;   zero if they are the same
;---------------------------------------------------------------

cmpLoc:
	; Compare value at HL with value at DE
	ldax	d
	cmp	m
	rnz

	; Increment HL/DE
	inx	h
	inx	d

	; Compare
	ldax	d
	cmp	m
	rnz

	; Increment HL/DE
	inx	h
	inx	d
	ldax	d
	call	secDiv
	mov	b,a
	mov	a,m
	call	secDiv

	; Compare after sector division. 
	cmp	b
	ret



	db 001H			;dead code


;---------------------------------------------------------------
; secDiv -- Subtract 1 from A,integer divide by 2
; in other words:  A = FLOOR((A-1)/2)
; 1 -> 0
; 2 -> 0
; 3 -> 1
; 4 -> 1
; 5 -> 2
; ...
; 31 -> 15
; 32 -> 15
;---------------------------------------------------------------
secDiv:
	dcr a
	ani 07EH
	rrc
	ret


;---------------------------------------------------------------
; memcpy3b - Copies 3 bytes from HL to DE. 
;---------------------------------------------------------------
memcpy3b:
	mvi	b,3H
	; Falls through to memcpy

;---------------------------------------------------------------
; memcpy - Copy routine.  Copies B bytes from HL to DE.
;---------------------------------------------------------------
memcpy:  
	mov	a,m		;Copy value pointed by HL to DE
	stax	d
	inx	h		;HL++
	inx	d		;DE++
	dcr	b		;B--
	jnz	memcpy		;If 
	ret

;---------------------------------------------------------------
; dispMsg - Prints message in HL via conOut.  Stops on '$' 
;---------------------------------------------------------------
dispMsg:
	mov	a,m
	cpi	024H
	rz
	mov	c,m
	inx	h
	push	h
	call	uconout
	pop	h
	jmp	dispMsg

;---------------------------------------------------------------
; upDPrm - Update disk paramater
; Called by selDsk
; drive number is in A
;---------------------------------------------------------------
upDPrm:
	lda	cpmDrv
	lxi	h,trkMax
	add	l  
	mov	l,a		;HL now points to trkMax[drive]
	mov	c,m		;c now has the max track for drv

	; Loop through dpLookup table, searching for the entry
	; corresponding to the maximum track number
	lxi	h,dpLookup 
udp010:
	mov	b,m
	mov	a,m
	ora	a           
	rm			;last entry of dpLookup is 0x80

	; Load 4 byte from dpLookup into b,e,d,a
	inx	h
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	mov	a,m
	inx	h 
	cmp	c		;a == max track for drv ?
	jnz	udp010		;no,keep looping
    
	; If we get here, byte 4 is max track for drv
	mov	a,b
	sta    dParam		;Store first byte as new SPT
	xchg			;H=Byte 3,L=Byte 2
	shld   dsmPtr		;Update DSM to Byte 2 and 3
				;For the case of 77 track drive
				;Updates DSM to 95h
	ret
	
    
	; Dead code
	call	readHst - 1
rstFlgs:
	xra	a
	sta	hstwrt
	sta	hstact
	sta	unacnt
	ret

;---------------------------------------------------------------
; dskIO - Multipurpose disk routine.  All disk operation pass
; through here.  Specifically this gets called for reading and
; writing from rwOper.
;
; On entry, A is set to the desired operation:
;   dskOpRd(1) -- Read sector
;   dskOpWr(3) -- Write sector
;
;   Drive, Track, Sector located in bufDrv, bufTrk, bufSec
;	(we set hostDrv, hostTrk, hostSec within this func)
;
;   Payload in buf
;
; This function appears to be analagous in many ways to dskIO
; in the micropolis driver code,but there are some serious
; differences.
;---------------------------------------------------------------
dskIO: 
	sta	diskOp		;Save A as diskOp

	; Check for "single disk mounting" option which allows
	; multiple disks to be used in a single drive.
	lda	flags
	ani	fMNTDSK		;single disk mounting?
	; bufDrv contains the drive for the current buffer
	lda	bufDrv		

	; Call askUserDisk if "single disk mounting" flag is set
	; If so, a will be set to 0.
	cnz	askUserDisk
	
	; Regardless, a now contains the drive we're using.
    
	; 
	; Copy buffer drv/trk/sec position to host drv/trk/sec.
	; Note that part of this involves dividing the sector
	; number by 2, as two CP/M sectors fit in one host
	; sector
	;
	sta	hostDrv		;set physical/host drive
	lhld	bufTrk		;L=bufTrk,H=bufSec
	mov	a,h             
	mov	h,l		;h=bufTrk

	call	secDiv		;a = floor((bufSec-1)/2)
	mov	l,a
	shld	hostSec		;store hostSec, hostTrk
	
	di			;disable interupts during IO

	call	dskIO0
	push	psw
	
	; Check if enable-interupts after disk access is enabled
	lda	flags
	ani	fENAINT
	jz	ds005
	ei			;Yes.  Enable interupts.
ds005:
	pop	psw
	ret

askUserDisk:
	; askUserDisk   - If "single disk mounting" flag is true,
	; ask user to swap disk to the one specified in reg A.
	; If that disk was last encountered,returns zero.
	lxi	h,mntMsgDrv
	adi	'A'
	cmp	m		;compare drive in error msg with
	jz	askUD0		;drv passed in. return if equal
	mov	m,a		;different drive passed in
	lxi	h,mountMsg	;ask user to change disks.
	call	dispMsg
	call	uconin
askUD0:
	xra	a		;point to physical drive 0
	ret

dskIO0:
	; Load new disk IO stack pointer,and save old stack ptr.
	lxi	h,0
	dad	sp		;H now contains old stack pointer
	lxi	sp,stack	;Set stack pointer to disk stack
	push	h		;Old stack pointer is now top of
				;new stack

	; Check if track is valid
	lxi	h,trkMax 
	call	addDrvOff	;HL += hostDrv
	mov	a,m		;load value at trkMax + hostDrv
	lxi	h,hostTrk
	sub	m		;subtract hostTrk
	jm	error2		;negative? call error2

	; Select drive and seek track
	lhld	hostSec		;L=hostSec,H=hostTrk
	mov	c,h		;C=hostTrk
	mov	b,l		;B=hostSec
	call	slct		;select head
	call	seek		;seek to track
	

	lda	diskOp
	cpi	dskOpRd		; reading?
	jnz	ds010
    
	; If diskOp=dskOpRd (read) we skip ds010,
	; go straight to ds020.
	call	ds020
	jmp	exitDIO

ds010:
	; diskOp=dskOpWr3 (write and optionally verify)
	; This section (ds010-ds020) prepares for the write.
	cpi	dskOpWr		;diskOp must be dskOpWr
	jnz	error2		;Invalid parameter error.

	; Find the sector *before* our target sector.
	lda	hostSec
	dcr	a
	ani	0FH
	mov	b,a		;b contains sector before target

	; Check if pre-write read is enable (track verify)
	; The Pre-write Read forces the disk drivers to read the
	; sector physically preceding and on the same track as
	; each sector to be written. This is intended to ensure
	; that the position of the disk head is precisely as
	; intended,by making use of the fact that any seek error
	; would cause the sector format address to produce a
	; read error and force the drive to re-seek. 
	lda	flags
	ani	fTRKVFY
	mvi	a,2
	cnz	ds020		;Call ds020 if fTRKVFY on
				;this will set diskOp=2
	;Done with pre-write read.

	call	ldTrk		;load current track for drive 
	mov	a,m		;into register a
	sta	bufHdr		;store track in buf


	lda	hostSec		;load sector
	mov	b,a
	lda	headSel		;load 10h for upper head
				;or 0h for lower
	rlc
	rlc
	rlc
	ora	b
	sta	bufHdr+1	;set physical sector in buffer
				; to (headSel << 3) | hostSec
	mvi	a,dskOpWr	;we reset diskOp in ds020
	call	ds020
	jmp	exitDIO

	; A 3 level retry structure is provided as follows:
	; L1 -- If an error occurs,up to 5 retrys of the
	;	offending operation will be performed.
	; L2 -- If the level 1 retrys are not successful,the
	;	positioner will be stepped off track and back
	;	and the level 1 retrys will be performed. The
	;	level 2 retrys will be performed up to 4 times.
	; L3 -- If the level 2 retry procedure is not successful
	;	the unit will be deselected to unload the head
	;	then the unit will be reselected,the positioner
	;	will be recalibrated moved back; to the desired
	;	track and the level 1 and 2 retry procedures
	;	will be performed.  This will be done up to 3
	;	times.  If not succcessful, a permanent IO error
	;	 will result.

ds020:
	sta	diskOp 
	mvi	a,003H
	sta	l3Rtry
ds030:
	mvi	a,004H
	sta	l2Rtry
ds040:
	mvi	a,005H
	sta	l1Rtry
ds050:
	; Store ds060 on top of stack so that various
	; jumps below will return to ds060.
	lxi	h,ds060
	push	h

	;
	; Perform operation based on diskOp
	;
	lda	diskOp
	dcr	a
	jz	readAl		;if diskOp=1,readAl
	dcr	a
	jz	readCk		;if diskOp=2,readCk
	dcr	a
	jnz	error2		;if diskOp!=3,error
	
	; If diskOp=3,write and optionally verify
	pop	h
	call	wSect  
	lda	flags		;check write verify flag
	ani	fWRTVFY
	cnz	readCk		;call readCk if fWRTVFY is set.

ds060:
	; We've finished performing the op
	mvi	a,0
	rz			;success!

	; If we failed,we retry.
	; If we have more L1 retries remaining,just repeat.
	lxi	h,l1Rtry
	dcr	m
	jnz	ds050 

	; We're out of L1 retries. The next step is an L2 retry.
	; We step out and in one track. 
	call	reStep
	lxi	h,l2Rtry
	dcr	m
	jp	ds040		;and retry

	; We're out of L2 retries. The next step is an L3 retry.
	; We seek to track 0,lift head, lower head, and reseek.
	call	reSlct
	lxi	h,l3Rtry
	dcr	m
	jp	ds030		;and retry
    
	; Fall through to error1


; This code is a bit hacky:
; Depending the db 01H modifies the mvi a to be a lxi b
; effectively ignoring the mvi a.  So you can set the a
; value by choosing your entry point.

error1:
	; Error 1 -- we failed to find tk0, or failed dskIO
	mvi	a,1
	db	01H
error2:
	; Error 2 - invalid parameter
	mvi	a,2
	db	01H
error3:
	; Error 3 - we failed to select
	mvi	a,3
	db	01H
error4:
	; Error 4 - disk is write protected
	mvi	a,4
	sta	erflag		;save error flag 
	mvi	a,1
exitDIO:
	ora	a
				;Restore old stack pointer
	lhld	stack-2
	sphl
	ret

;---------------------------------------------------------------
; seek - Seek to track
; Destination track is contained in register C (from hostTrk)
; Destination sector is contained in register B (from hostSec)
; Apparently sector is used to select upper/lower head
; for double sided drives
;---------------------------------------------------------------
seek:
	mov	a,b
	ani	10H		;if sector number > 16, then
				;   it's located on second side
	sta	headSel		;store 10H for second side,
				;   or 0H otherwise
	call	slct
	; Drop through to seek0

;---------------------------------------------------------------
; seek0 - Seek to track
; Destination track is contained in register C 
;---------------------------------------------------------------
seek0:
	push	h
	call	ldTrk		;HL=track for current drive
	mvi	a,unkTrk
	cmp	m		;current track is unknown?
	cz	restor		;if yes,calibrate position 
	mov	a,c
	sub	m		;A = C - trkTbl[drv]
	jz	seekR		;zero? we don't need to seek
	jm	sekOut		;negative? step out A steps
sekIn:				;step in A steps
	call	stepIn
	dcr	a
	jnz	sekIn
	jmp	seekR1
sekOut:				;step out A steps
	call	stepOut
	inr	a
	jnz	sekOut
seekR1:
	push	d
	call	settle		;wait head settle time
	pop	d
seekR:
	mov	m,c		;store new track location
	pop	h
	ret
	
	db	006H		;dead code. not in driver.

;---------------------------------------------------------------
; stepIn - Step positioner in 1 track
;---------------------------------------------------------------
stepIn:
	push	psw
	push	d
	push	h
	lhld	FDCPTR
	mvi	m,STEP+1
stepIn0:
	lxi	d,0001EH
	call	timer
	pop	h
	pop	d
	pop	psw
	ret

;---------------------------------------------------------------
; stepOut - Step positioner out 1 track
;---------------------------------------------------------------
stepOut:
	push	psw
	push	d
	push	h
	lhld	FDCPTR
	mvi	m,STEP
	jmp	stepIn0

;---------------------------------------------------------------
; reStep - Step off track one and back to correct
; possible marginal track position of drive which
; wrote the disk.  If track 0 substitute restor
;---------------------------------------------------------------
reStep:
	call	ldTrk		;Get current track
	mov	a,m
	ora	a
	jnz	reStep0 
	; If current track is zero,we just do a restor
	jmp	restor
reStep0:
	; We alternate step-in/step-out and step-out/step-in
	lda	rstpDir
	cma
	sta	rstpDir
	ora	a
	jnz	reStep1
	call	stepIn
	jmp	stepOut
reStep1:
	call stepOut
	jmp	stepIn


;---------------------------------------------------------------
; reSlct - Retry routing to restore to 0 then
; lift head,lower head, and reseek
;---------------------------------------------------------------
reSlct:	
	push	h
	lhld	FDCPTR
	mvi	m,RESET		;Reset controller
	lxi	d,200 
	call	timer
	call	slct		;Reselect,lower head
	pop	h
	call	ldTrk		;HL points to current track
	mov	c,m		;copy current track to c
	call	restor		;Restore to track 0 
	jmp	seek0		;Seek back to track


;dead code.
	xra	a
	sta	headSel
	call	ldTrk
	mov	a,m
	ora	a
	rz

;---------------------------------------------------------------
; restor -- Restore positioner to track 0.
; Positioner must be stepped out
; until the track 0 switch is made 
; to calibrate track position
;---------------------------------------------------------------
restor:
	push	h
	push	b
	call	ldTrk		;HL=current track in trkTbl
	mvi	m,unkTrk	;reset to unknown track
	call	restr1  
	mvi	m,000H		;Set track=0 in trkTbl
	pop	b
	pop	h
	ret
restr1:
	; Restore to TK0
	push	h
	call	slct		;Ensure unit selected and ready
	push	d
	push	b
	lhld	FDCPTR
	inx	h		;HL points to status register
	mov	a,m		;Read status
	ani	TK0		; Check track 0
	jz	rest3		;No -- press on
	; Already at track 0 - Step
	; in 8 times then restore
	; to ensure good position
	mvi	a,8
rest2:
	call	stepIn		;Step in 8
	dcr	a		;tracks
	jnz	rest2
	call	settle		;Wait settle time
rest3:
	; Step out until track 0 switch
	; is actuated or until 85 steps
	; have been issued so that we
	; don't bang against the stop
	; forever if the switch is
	; broken
	mvi	c,85		;Load max stepcnt
rest3A:
	mov	a,m		;Track 0?
	ani	TK0	
	jnz	rest4		;Yes -- press on
	call	stepOut		;Step out one tk
	dcr	c		;Max steps?
	jnz	rest3A		;No -- try again.

	; Maximum number of steps have
	; been issued - error abort
	jmp	error1
rest4:
	call	settle
	pop	b
	pop	d
	pop	h
	ret

;---------------------------------------------------------------
; ldTrk - Load address of current track on current unit into HL
;---------------------------------------------------------------
ldTrk:
	lxi	h,trkTbl
addDrvOff:
	lda	hostDrv
	add	l 
	mov	l,a
	ret

;---------------------------------------------------------------
; slct - Select current specified drive 
; all registers ignored on entry.
;---------------------------------------------------------------
slct:
	push	d
	push	b
	push	h
	mvi	d,000H

	;	
	; set headSl1 to 10H if headSel is nonzero,otherwise 0H
	; previous headSl1 is stored in e for a later check
	;
	lda	headSl1
	mov	e,a 
	lda	headSel
	ora	a
	jz sl00
	mvi	d,010H
sl00:
	mov	a,d
	sta	headSl1
	; done with headSl1 logic

	lhld FDCPTR
	lda	hostDrv 
	ani	003H
	mov	b,a
	ora	d
	mov	d,a		;d = headSl1 OR hostDrv
	inx	h		;point hl to status/command reg
	mov	a,m		;read status
	mov	c,a		;store status in c
	mov	a,d		;a = headSl1 OR hostDrv
	ori SLUN		;a = SLUN OR headSl1 OR hostDrv
	mov	m,a		;Write command

	; In this next code we compare the previous values of
	; head selected,upper/lower head, and drive number
	; with the new values.
	
	mov	a,c		;status value before command
	ani	007H		;mask USLT and drive addr
	ora	e		;add previous headSl1 bit         
    
	; A now contains previous version of:
	; headSl1 bit OR USLT bit OR drive number

	; D already contains headSl1 OR cpmDrv
	; we don't need to worry about new USLT
	; since USLT=0 when selected,and the new value
	; is following selection.
	xra	d		;Compare new/old values
	mov	a,c
	
	jz	sl15		;If same,skip timer

	; The new/old values were different.
	; This means we changed either
	;   - drive number
	;   - selected (head loaded) or not
	;   - upper or lower head (for double sided)
	; so we need an appropriate delay

	ani	07H		;Get USLT and address again
	xra	b		;compare with hostDrv
	; Now zero status represents either
	;   - different drive number
	;   - selected (head loaded) or not
	; Zero means unchanged

	lxi	d,1H
	jz	sl10

	; If we get here drive number and/or selected
	; has changed.
	push	h
	lxi	h,5000
sl05:
	lxi	d,1
	call	timer
	dcx	h
	ora	h
	jm	error3
	xthl
	mov	a,m
	xthl
	ani	20H
	jz	sl05

	pop	h
	lxi	d,250

sl10:
	call	timer		;Get in sync
	mov	a,m		;Get status
	ani	007H		;Error if not selected
	xra	b		;Ensure unit is ready
	mov	a,m		;slct returns status in A
	jnz	sl20		;return
sl15:
	ani	RDY		;Ensure unit is ready
	xri	RDY
sl20:
	pop	h
	pop	b
	pop	d
	rz			;RDY bit is true? success
	jmp	error3		;failure
settle:
	lxi	d,10

;---------------------------------------------------------------
; timer - 1 millisecond timer.
; DE = Delay time in ms
;---------------------------------------------------------------
timer:
	push	b
	push	h
	lhld	FDCPTR
	mov	a,m		;Trigger a timer
	ani	DTMR
	mvi	a,96
	jnz	ti05
	rlc
ti05:
	mov	b,a
ti10:
	mov	a,b
ti15:
	sui	1
	ora	a
	jnz	ti15
	dcx	d
	mov	a,e
	ora	d
	jnz	ti10
	pop	h
	pop	b
	ret


;---------------------------------------------------------------
; wSect - Write 1 sector
; Drive is already in position with seek.
; B contains sector number
; buf contains desired data
;---------------------------------------------------------------
wSect:
	call	slct		;Ensure unit selected
	push	b
	mvi	c,(secLen-2)/2	;we count pairs of bytes
	lhld	FDCPTR
	push	h
	inx	h		;h contains status/cmd reg
	mov	a,m		;read status
	ani	WPT		;check write protect
	jnz	error4		;disk is write protected
	lxi	d,bufHdr
	dcx	h
	call	getSec		;wait for desired sector
	mvi	m,WTCMD		;send write command to ctrl
	inx	h
    
	; Wait for transfer flag
ws010:
	ora	m
	jp	ws010
    
	; Write sync byte
	inx	h
	mvi	m,syncByt

	xra	a		;clear carry
	xchg    
	mvi	b,0		;clear checksum

ws020:
	mov	a,m		;get byte from mem
	stax	d		;write to disk
	adc	b		;add to cksum
	mov	b,a		;save cksum
	inx	h		;next byte
	mov	a,m		;-etc- second byte
	stax	d
	adc	b
	mov	b,a
	inx	h
	dcr	c
	jnz	ws020		;loop until 2*C bytes written

	; Write checksum
	mov	a,b
	stax	d

	; Wait until end of sector
	pop	h
	xra	a

ws030:
	ora	m
	jp	ws030		;Wait sector flag

	; Wait 1 msec for erase delay
	lxi	d,1H
	call	timer
	pop	b
	xra	a
	ret



;---------------------------------------------------------------
; readAl - Read 1 Sector
;   Verify checksum and header
;   Destination buf
;   Returns Z=OK
;           NZ=error
;   Desired sector in B
;---------------------------------------------------------------
readAl: 
	call	slct
	push	b
	mvi	c,(secLen-2)/2
	call	wtSync		;HL now is FDCPTR + 2
	xchg
	mvi	b,0
rda010:
				;DE contains FDCPTR+2
	ldax	d		;read from disk
	mov	m,a		;move to buffer
	inx	h		;next loc
	adc	b		;add to checksum
	mov	b,a		;and save
	ldax	d		;next read
	mov	m,a		;-etc- (2nd byte)
	inx	h
	adc	b
	mov	b,a
	dcr	c		;end of data?
	jnz	rda010		;no - loop

	ldax	d
rda020:
	cmp	b		;compare with
	pop	b		;computed checksum
	rnz			;return if error
	call	ldTrk		;point HL at entry in trkTbl
	lda	bufHdr		;load read track number
	cmp	m		;does track number at HL agree?
	rnz			;no,return
	lxi	h,bufHdr+1
	lda	headSel
	rlc
	rlc
	rlc
	ora	b		;a = (headSel << 3) | B
	cmp	m		;compare bufHdr+1 with A 
	ret

;---------------------------------------------------------------
; readCk - verify sector
; Read through sector without moving data into memory and verify
; track and sector in checksum. Only track and sector ID are
; read into memory and checksum is verified.
; Sector is specified in B Reg
; Returns   Z=OK
;           NZ=ERROR
;---------------------------------------------------------------
readCk:
	call	slct
	push	b		;save sector
	mvi	c,(secLen-4)/2
	call	wtSync		;wait for sector,strip off sync
				;and set D to point to buf
	mvi	b,0		;clear checksum
	mov	a,m		;read track ID
	stax	d		;save in buffer
	adc	b		;add to checksum
	mov	b,a		;and save.
	inx	d		;increment buffer pointer
	mov	a,m		;read sector ID
	stax	d		;save in buffer
	adc	b		;add to checksum
	mov	b,a		;save
	nop
rdck10:
	mov	a,m		;read from disk
	adc	b		;add to checksum
	mov	b,a		;save checksum
	nop			;timing?
	nop			;timing?
	mov	a,m		;-etc- 2nd byte
	adc	b
	mov	b,a
	dcr	c
	jnz	rdck10
				;end of data
	mov	a,m		;read checksum
	jmp	rda020		;check header and checksum

;---------------------------------------------------------------
; wtSync - Wait for desired sector to come around
; and strip off the sync byte for read routines.
; Desired sector in B
; HL set to FDCPTR+2
;---------------------------------------------------------------
wtSync:
	lxi	d,bufHdr
	lhld	FDCPTR
	call	getSec		;wait for next sector
	inx	h		;HL now points to status reg

wts010:
	ora	m
	jp	wts010		;loop until xfer flag is set
	
	inx	h
	mov	a,m		;read sync byte
	xra	a
	ret

;---------------------------------------------------------------
; getSec- Wait for desired sector to come around.
; HL must point to FDCPTR
; Desired sector in B
;---------------------------------------------------------------
getSec:
	mov	a,m		;wait for SCTR flag
	ora	a
	jp	getSec
	ani	00FH		;see if this is the sector
	xra	b		;   we want?
	rz			;yes -- return
	jmp	getSec		;no -- try again


;---------------------------------------------------------------
;
;	Begin Parameters
;
;---------------------------------------------------------------

; Parameter area in the 12 bytes before the user area
	ORG usrBase - 12
stkPtr:
	dw	stack
	db	1Bh,0h,0h,0h	;unknown purpose (if any)

; Maximum tracks per drive.
; This can be changed to support combinations of MOD-I and
; MOD-II drives.
trkMax	db	4Ch,4Ch,4Ch,4Ch
        db	1Bh		;unknown?

flags	db	0C0h		;flags, as defined below

fCLDCMD	equ	01h		;CCP process cmd on cold start
fWRMCMD	equ	02h		;CCP process cmd on warm start
fMNTDSK	equ	04h		;single disk mounting
fENAINT	equ	10h		;enable interrupts after disk IO
fWPTDET	equ	20h		;write-protect detect
fWRTVFY	equ	40h		;write verify flag
fTRKVFY	equ	80h		;force track number verification


; Parameter area above user area
	ORG prmBase 
FDCPTR	dw	0h
trkTbl	db	unkTrk, unkTrk, unkTrk, unkTrk
erflag	db	0		;disk error code

mountMsg
	db	0Dh,0Ah
	db	'Mount disk '
mntMsgDrv
	db	'A then <CR>'
	db	0Dh,0Ah, '$'

; Skew table mapping logical sectors to physical sectors
xlate	db	01,02,11,12,21,22,31,32,09,10,19,20,29,30,07,08
	db	17,18,27,28,05,06,15,16,25,26,03,04,13,14,23,24
	db	01		; Last entry must loop back to 1

cldDone	db	0

; dpLookup - disk parameter lookup table
; Used to map the number of tracks on the drive 
dpLookup
	; Sector Per Track,DSM LSB, DSMMSB, Max Track
	db	20h,41h,0h,22h	;35 track drive
	db	20h,95h,0h,4Ch	;77 track drive
	db	40h,83h,0h,0A2h	;163 track drive
	db	40h,2bh,1h,0cch	;205 track drive
	db	80h		;end mark

; dpHead - disk parameter header for each drive
dpHead	dw	xlate,0,0,0,dirBuf,dParam,csv0,alv0
	dw	xlate,0,0,0,dirBuf,dParam,csv1,alv1
	dw	xlate,0,0,0,dirBuf,dParam,csv2,alv2
	dw	xlate,0,0,0,dirBuf,dParam,csv3,alv3

dParam	dw	SPT
	db	BSH
	db	BLM
	db	EXM
dsmPtr	dw	DSM
	dw	DRM
	db	AL0
	db	AL1
	dw	CKS
	dw	OFF

cpmDrv	ds	1
cpmTrk	ds	1
cpmSec	ds	1		;cpm current sec, 1 based
bufDrv	ds	1		;drive number for current buf
bufTrk	ds	1		;track number for current buf
bufSec	ds	1		;sec number for current buf
wrtDrv	ds	1		;drive number for write
wrtTrk	ds	1		;track number for write
wrtSec	ds	1		;sec number for write
hstact	ds	1		;Also called HSTACT in CP/M ref
hstwrt	ds	1		;Also called HSTWRT in CP/M ref
rsFlag	ds	1		;Read sector flag
readOp	ds	1
wrType	ds	1		;Set by reg C in write
unacnt	ds	1
dmaAddr	ds	2
diskOp	ds	1		;Local variable to dskIO
hostDrv	ds	1		;Disk currently selected on ctrl
hostSec	ds	1
hostTrk	ds	1
l1Rtry	ds	1
l2Rtry	ds	1
l3Rtry	ds	1
rstpDir	ds	3
headSel	ds	1		;Upper or lower head for
				;   double sided drives
headSl1	ds	1
dirBuf	ds 	128

; Above this point in memory CP/M doesn't load or init.
alvLen	equ	26H

alv0	ds	alvLen
csv0	ds	CKS
alv1	ds	alvLen
csv1	ds	CKS
alv2	ds	alvLen
csv2	ds	CKS
alv3	ds	alvLen
csv3	ds	CKS

buf:
bufHdr	ds	12		;track num,sec num, 10 unused
bfData1	ds	cSecLen		;first 128-byte half
bfData2	ds	cSecLen		;second 128-byte half


