JUNGLE-BOARDGAME

Created by Maximiliam Münzen, Phuoc Thang Le and Anna-Maria Dickmann

This project is about the implementation for the chinese boardgame jnugle, also called animal chess.

Description:
Animal Chess is a chess-like board game for two players. At the beginning each player is assigned one of 
the  two  colors,  either  red  or  black.  The  board  consists  of  7x9  squares  and  represents  a  jungle.  In  this 
jungle there are various animals, represented by animal tokens which can move through different terrain. 
The game’s goal is either to get one of your own animals into the opponent's den or to capture all of the 
opponent's animal tokens. 

Setup:
To start the game you have to start the start-ui.rkt file. Then you can select a game mode and start a server. If u want to play the game in multiplayer one party has to start a server via the UI. Afterwards he needs to select multiplayer game like his opponent. 
To start in local mode just press "Local play".

Development hints:
To test the multiplayer function localy you can start the multiplayer game and type in the field for the Ip-adress "DEBUG". 
The core game and grpahical logic is in the client.rkt file.
Its possible to change constants in the "Constants.rkt" files (like player images, figure images etc.).
The serverside is described in the Server.rkt file.
To change the communication with the server you have to look up in the "network.rkt" file
