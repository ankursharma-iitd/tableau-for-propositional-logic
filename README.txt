This is the README file for Analytics tableau for ->
    Entry Number: 2015CS50278, and 
    Name: Ankur Sharma.

Files:
1. sig.mli -> contains the signature of the file
2. arg.ml -> contains the arguments to be passed into the tableau
3. str.ml -> runs the actual tableau
4. run.sh -> To run the tableau, and obtain the graph simply run the bash file : sh run.sh
5. mygraph.dot -> DOT file created as a result of running the tableau
6. mygraph.png -> Image file created which gives the pictorial representaion of the entire tableau tree

Note:
1. Module names can't start with numbers, hence they have been named simply as 'sig.mli', 'arg.ml', 'str.ml', etc.
2. In the Final Graph Image, -123456 denotes all the CLOSED PATHS, whereas 123456 denotes all the OPEN PATHS in the tableau.
3. The Validity of arguments, and the false assignments in case the argument is invalid has been printed on the terminal line.