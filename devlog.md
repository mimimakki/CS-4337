## March 11, 2025 | 2:10 PM 

I will be creating a prefix-notation expression calculator using Racket. The user will be prompted to input an expression in prefix-notation, and my program will calculate the result
and output it to the user. The calculator will contain a history feature, where previous (and successful) calculations will be kept and stored. The program will also handle potential
errors. 

## March 25, 2025 | 3:40 PM 

So far, I have included the code the professor has provided for the program to be run either in batch or interactive mode. I have also created a function to parse the input from user. 

## March 26, 2025 | 12:38 AM

I have renamed the function ```parse-input``` to ```process-tokens``` for naming clarity purposes. I also simplified it and added more comments for readability purposes. I created a new function to tokenize the input which handles each case for the calculator. Next, I need to evaluate the expression using the tokens. 

## March 27, 2025 | 12:27 AM

I have created three evaluation functions. ```eval-operation``` is a helper function that is called repeatedly for each operator within the ```eval-expressions``` function. The ```eval-loop``` function is what will be called within main to continuously prompt the user to enter an expression for calculation. ```eval-loop``` will display the history id followed by the calculated value, unless there is an error. I still have to handle a few errors and terminate the program when the user inputs "quit". I also have to figure out if the mode detection works when the program is run through terminal.  

## March 27, 2025 | 2:25 PM

I handled a few error checks taken place in ```eval-operation``` including division by zero and invalid operands (i.e. a letter). The program will be terminated if the user inputs "quit". The mode detection runs smoothly through terminal; per the instructions, only the results and errors are displayed in batch (no prompts). 

## March 27, 2025 | 4:50 PM

I tweaked my code a bit to stop further processing if there is an error (i.e. invalid token) found when tokenizing. I also re-read through the project instructions and "– A unary operator that negates the value of an expression. There is no subtraction. To subtract we can add a negative number." So, I changed the logic for the - operator to follow negation instead of subtraction. 
