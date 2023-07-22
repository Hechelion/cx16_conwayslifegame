# cx16_conwayslifegame
Conways game of life for cx16.

Title: Conway game life
Language: Assembly
System: Commander X16
Version: Emulator R43
Author: Hechelion (hechelion@gmail.com)
Date: 2023-07-04
Compiler: CC65
Build using:	cl65 -t cx16 -o CONWAYGL.PRG -l conwaygl.list conwaygl.asm

#How to play
LOAD "CONWAYGL.PRG",8
RUN

#Controls
Q - Exits game.
1 - Set 60 FPS simulation speed
2 - Set 6 FPS simulation speed
3 - Set 2 FPS simulation speed
4 - Set 1 FPS simulation speed
5 - Pause ans step*step forwards

#Description
a 40*30 grid simulation of Conways game of life para el sistema CX16.
