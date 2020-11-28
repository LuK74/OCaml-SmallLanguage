# PROJET-LTPF

## Team
CHALOYARD Lucas   
HERQUE Eric   
VACHERIAS Guillaume   

## Project
https://ltpf.gricad-pages.univ-grenoble-alpes.fr/commun/projet/sos.pdf

## Overleaf
https://www.overleaf.com/4185181216ykhccbpvsvvx

## Our language
- Affectation :   
```pascal
    id := 1
```
- Operators :    
=, <, <=, >=, >, ||, &&, ! are available
   
- While : 
```pascal
    while (id = 1) {
        id2 := id2 + 1
    }   
```
   
- If : 
```pascal
    if (id = 1) {
        id2 := true
    } else {
        id2 := false
    }
```
   
- Int operators :
+, -, *, / are available (operations priority are not implemented yet, it'll starts from the right)
```pascal
    id := 1 + 2 * 3 - 4
```
Id will be equal to -1 (1 + 2 * (-1) --> 1 + (-2) --> -1)
   


## How to use
To compile :  
```bash
    make
```
    
To execute :
```bash
    bin/analyser
```

Test prog are in src/config.ml 
How to use "automatedTest" :  
```ocaml
    (* string corresponding to the prog *)
    let str1 = "while(a){a:=0;while(b){b:=0;while(c){c:=0;while(d){d:=0}}}}"
    (* just call automatedTest, it will return the final state, but also print it *)
    let _ = automatedTest str1
```

How to use the debugger :
```
    (debug) next  #Execute one step
    (debug) print  #Print the current state (value of variables during execution)
    (debug) continue  #Continue until the end of the program
```
