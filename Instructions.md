# 1. Extract data from TXM

- For NCA4, use concordance (corpus not built correctly for use with Index command)
- Set View \#1 to display following properties:
	- word
	- pos
	- ttpos
 	- rnnpos
 	- ttlemma
 	- rnnlemma
 	- subcorpus_id
 	- s_line
 	- (position)
- Set left and right contexts to zero
- For elision, CQL queries are:
	- JO
	 - ```[pos="PRO:pers:suj:1:sg.*" & word="(j|i|g)(e|o|ou|eo)"] [word="[aeiouyáéíóúàèìòùâêîôûäëïöü].*"]```
	 - ```[pos="PRO:pers:suj:1:sg.*" & word="(j|i|g)"] [word="[aeiouyáéíóúàèìòùâêîôûäëïöü].*"]```

- Run script Clean_data_NCA.sh
	- At first run, make it executable:
  		- open a terminal where script is
  		- enter ```chmod +x Clean_data_NCA.sh```
  		- then the script can be run with ```./Clean_data_NCA.sh [file name]```
  		- or ```.././Clean_data_NCA.sh [file name]``` if located in the repertory above the current one.

# 2. Run ```src/main.r```

This script is quite self-explanatory and use GUI.
