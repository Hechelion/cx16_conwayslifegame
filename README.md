# cx16_conwayslifegame
Conways game of life for cx16.

>Title: Conway game life screensaver<br/>
>Language: Assembly<br/>
>System: Commander X16<br/>
>Version: v1.0.1<br/>
>Emulator: R43-R46 (R45 need a update bios)<br/>
>Author: Hechelion (hechelion@gmail.com)<br/>
>Date: 2023-08-01<br/>
>Compiler: CC65<br/>
>Build using:	cl65 -t cx16 -o CONWAYGL.PRG -l conwaygl.list conwaygl.asm<br/>


## How to play
>LOAD "CONWAYGL.PRG",8<br/>
>RUN

## Controls
>Q - Exits game.<br/>
>R - Reset game.<br/>
>D - Alternate Demo mode.<br/>
>1 - Set 60 FPS simulation speed<br/>
>2 - Set 12 FPS simulation speed<br/>
>3 - Set 6 FPS simulation speed<br/>
>4 - Set 2 FPS simulation speed<br/>
>5 - Pause ans step*step forwards<br/>


## Description:
a 40*30 grid simulation of Conways game of life for CX16 8 bit computer.<br/>
The game starts in demo mode (screensaver), in this mode the game will automatically restart every 255 cycles.
