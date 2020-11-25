# PROJET-LTPF

## Team
CHALOYARD Lucas   
HERQUE Eric   
VACHERIAS Guillaume   

## Project
https://ltpf.gricad-pages.univ-grenoble-alpes.fr/commun/projet/sos.pdf

## Overleaf
https://www.overleaf.com/4185181216ykhccbpvsvvx

## How to use
To compile :  
```bash
    make
```
    
To execute :
```bash
    bin/analyser
```

Test prog in src/config.ml   
How to use "automatedTest" :  
```ocaml
    (* string corresponding to the prog *)
    let str1 = "while(a){a:=0;while(b){b:=0;while(c){c:=0;while(d){d:=0}}}}"
    (* just call automatedTest, it will return the final state, but also print it *)
    let _ = automatedTest str1
```

How to use the debugger :
```bash
    (debug) next #Execute one step
    (debug) print #Print the current state (value of variables during execution)
    (debug) continue #Continue until the end of the program
````
