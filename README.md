# SER502-Spring2021-Team2
Vertex Language ( Lexical Analyzer, Parser, Runtime ) By: Piyush Reddy Mudireddy, Ritesh Reddy Anugu, Amulya Bodla, Niharika Pothana.

Vertex Programming Language
System Specifications
Compiler and Runtime Environment:

[Operating System] - Windows 10
[Processor] - Intel(R) Core(TM) i5-8265U CPU
[Memory] - >= 4 GB RAM

Tools Used:
Prolog - A Declarative Programming Language for implementing the compiler and runtime environment.

Installation
[SWI-Prolog] 8.2.4-1 is used to run the program. This can be downloaded using the below link:
https://www.swi-prolog.org/download/stable

Download the code from the repository:
$ git clone https://github.com/Prmudire/SER502-Spring2021-Team2
$ cd SER502-Spring2021-Team2

Build/Run Instructions
1) Open SWI-Prolog runtime environment.
2) Consult both the files, ParseTreeGenerator.pl and Evaluator.pl.
3) Run the predicate vertex("<FILE_PATH/FILE_NAME>.vtex"). 
4) The .intc file is created in the same directory as the input file.
5) The code output is generated automatically.

Sample Run
For Windows:

Step 1 and 2 are consult one-time runs
1) consult("C:\Users\Piyushred\Downloads\502\Vertex\ParseTreeGenerator.pl"). (Provide local filePaths)
2) consult("C:\Users\Piyushred\Downloads\502\Vertex\Evaluator.pl").
3) vertex("C:\Users\Piyushred\Downloads\502\Vertex\megaSample.pl").

Features
1) Implemented primitive data types types: int, bool, string.
2) Support for addition subtraction, multiplication and division operations on int datatype.
3) Supports bool comparators: AND, OR, NOT
4) Supports iterative statements( for, for-in-range, while).
5) Supports conditional statements( if-else, if-then-else, ternery).
6) Supports display statements (display, displayln).
7) Intermediate code is generated and saved with an .intc file extension.
8) Evaluator takes this .intc file as input and generates the ouput.

Additioinal Features Implemented
1) Implemented strongly typed datatypes.
2) Supports nested block structures.
3) Supports Increment(++) and Decrement(--) operators for integers.
4) Supports comprator operations("<", "<=", ">", ">=", "==", "~"). (~ is not equals operator).
5) Supports paranthesis within expressions.
6) Supports composite boolean expressions (expression1 OPERATOR(AND/OR) expression2).

Team Members
1) Amulya Bodla (abodla)
2) Niharika Pothana (npothana)
3) Piyush Mudireddy (prmudire)
4) Ritesh Reddy Anugu (ranugu)
