CC :=cl65
CFLAGS :=-t cx16

CONWAYGL.PRG: conwaygl.asm
	$(CC) $(CFLAGS) -o CONWAYGL.PRG -l conwaygl.list conwaygl.asm
