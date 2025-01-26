*Luke Hodges*

### Overall Grade: 119/130

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format html and pdf? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

All good.

### Completeness, correctness and efficiency of solution: 74/80

- Q1 (10/10)

	Is the GitHub.com repository name set up correctly? If using name other than `biostat-203b-2025-winter`, take 5 points off.

- Q2 (20/20)

	If CITI training is not completed successfully, take 15 points off. 
	
	If PhysioNet crecential is not complete, take 5 pts off.

- Q3 (15/20)

	Q3.1, if the gz files are ever decompressed or copied in the solutions, take 5 points off.
	
	For Q3.5-7, should skip the header when finding the unique values of each variable. Take 5 points of if not done so.

  - Q3.3. `zcat` does not fully decompress the file; it displays the decompressed content directly without decompressing the file. `-1.0`

  - Q3.4. Clarify what the bash script is doing for `What’s the output of the following bash script?`. `-1.0`

  - Q3.6. Counts for each unique value of these variables. Your solution outputs the number of variables. Use `sort -k 1 -n -r` `-2.0` 

  - Q3.7. The number of unique patients in `icustays.csv.gz` not in `admissions.csv.gz` is 65366. Use `'{print $1}'`. `-1.0`

  - Q3.8. Delete the decompressed file within the script. `-0.0`

- Q4 (9/10)

	It's fine to just count the lines containing each name. If a student figures out a way to count the words (one line may contain the same name multiple times), give bonus points.

  - Q4.3. The explanation of `$3` is unclear. The output is the print from the 16th to the 20th. `-1.0`

- Q5 (10/10)

- Q6 (10/10)
	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out, not clustered the day before deadline. 
          
-   Is the hw1 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put auxiliary files into version control. If files such as `.Rhistory`, `.RData`, `.Rproj.user`, `.DS_Store`, etc., are in Git, take 5 points off.

-   If those gz data files or `pg42671` are in Git, take 5 points off.

Double `middle.sh` when you tagged the submission. But I know you deleted one of them outside `hw1` later. `-0.0`

### Reproducibility: 5/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Q4.3. `Quitting from lines 352-353 [unnamed-chunk-30] (Homework1.qmd)` in rendering on my laptop. You should have put `{bash, eval=F}` `-5.0`
    
    Q4.3. Also, `~/middle.sh pg42671.txt 20 5` stops rendering on my laptop. Write `./middle.sh pg42671.txt 20`. `-0.0`

### R code style: 20/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

No violations.

-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
