# PROJET-LTPF

## Team
CHALOYARD Lucas   

## Project
https://ltpf.gricad-pages.univ-grenoble-alpes.fr/commun/projet/sos.pdf

## Our language
[NEED TO BE REVIEWED] : ArithExp & BoolExp grammar, and syntax analysis.      
[NEED TO BE REVIEWED] : Project structure (usage of module, etc...).        


- Affectation :   
```pascal
    id := 1
```
- Operators :    
=, <, <=, >=, >, ||, &&, ! are available
   
- While : 
```c
    while (id = 1) {
        id2 := id2 + 1
    }   
```
   
- If : 
```c
    if (id = 1) {
        id2 := true
    } else {
        id2 := false
    }
```
   
- Int operators :
+, -, *, / are available (operations priority are not implemented yet, it'll starts from the right)
```c
    id := 1 + 2 * 3 - 4
```
Id will be equal to -1 (1 + 2 * (-1) --> 1 + (-2) --> -1)
   
[UPDATE] : Parenthesis have been add, BUT we have to surround each operation between 2 expressions  
Example : 1 + 2 * 3 - 4 ========> (1 + ((2 * 3) - 4))   
But now operations priorities are respected.

[UPDATE] :
- '//' :
```c
    a := 0 // a := 1
```
This operator simulate an concurrent execution (those instructions will be executed in a random order).    
## How to use
To compile :  
```bash
    make
```
    
To execute :
```bash
    bin/analyser
    bin/analyserN
```
Analyser : Use Structural Operational Semantics     
AnalyserN : Use Natural Semantics      

Test prog are in src/config.ml         
How to use "automatedTest" :  
```ocaml
    (* string corresponding to the prog *)
    let str1 = "while(a){a:=0;while(b){b:=0;while(c){c:=0;while(d){d:=0}}}}"
    (* just call automatedTest, it will return the final state, but also print it *)
    let _ = automatedTest str1
```
At the end of every test, the number of step required is return. (While instructions count for 2, and Skip (end of sequence) count for 1 even if it doesn't do anything). This will be change later.        

And how to use the debugger for "automatedTest" :        
```
    (debug) next  #Execute one step
    (debug) print  #Print the current state (value of variables during execution)
    (debug) continue  #Continue until the end of the program
```
How to use "Interactive Execution" :        
During the time of the execution, you'll be allocated one state. Every instructions are applied on this state.       
Instructions can be simple as : " id := 1 ", or a complete program like :       
" a:=1;b:=1;if(b = a) { b := 2 } else { a := 2} "       
```
    > [waiting for an instruction]
```
