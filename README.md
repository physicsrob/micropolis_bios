# CP/M 2.2 BIOS for Micropolis 
This repository contains a disassembled Lifeboat 2.2 CP/M for Micropolis BIOS.  It should be usable for computers with a Micropolis or Vector Graphic floppy controller board and appropriate disk drives.


# Using this BIOS
To modify an existing CP/M system to use this BIOS:
1. Modify `memSize` to match the system
2. Assemble.
3. Use `MOVCPM` to generate a CP/M image
4. Patch the CP/M image with 
```
	A>DDT CPMxx.COM
	-IBIOS.HEX
	-Rxxxx		(where xxxx = BIAS computed below)
	-G0		(Go zero, not "oh")
```
5. The patches CP/M image will be contained in memory.  Write it to disk by executing `SYSGEN`

# Physical Drive Layout
| Length | Content |
| ------ | ------- |
| 1      | Sync byte (always 0xFF) |
| 1      | Track Number (0-76 for mod-II drive, 0-35 for mod-I drive)|
| 1      | Sector number (0-15) |
| 10     | Unused |
| 128    | First CP/M logical sector payload |
| 128    | Second CP/M logical sector payload |
| 1      | Checksum |


# References
1. Original machine code was extracted from CP/M Lifeboat 2.2 for Micropolis disk image, located at https://deramp.com/downloads/vector_graphic/software/disk_images/Micropolis%20controller/56K%20Bootable%20Disk%20Images.zip
2. [Micropolis Driver](https://deramp.com/downloads/vector_graphic/software/manuals/Disk%20Driver.pdf).  		The code for the micropolis driver was 		incredibly helpful.  Some of the disk logic was		exactly the same as in the driver.  Where		possible I copied routine names and comments.  In other cases the logic here is quite modified.  For instance this driver code supports double sided disks.
3. [CP/M Operating System Manual](http://www.gaby.de/cpm/manuals/archive/cpm22htm/).  Section 6,in general, but especially Appendix G. The read/write blocking and deblocking seems to be heavily inspired by the manual example.  ;