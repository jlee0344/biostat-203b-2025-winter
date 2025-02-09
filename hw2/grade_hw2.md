*Julie Lee*

### Overall Grade: 175/180 (great!)

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format (html, pdf)? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

All good.

### Completeness, correctness and efficiency of solution: 125/130

- Q1 (19/20)

    - Q1.1 Nice summary and using `stringsAsFactors = TRUE` is smart. `+2.0`
    
    - Q1.2 The memory usage should be mush smaller with about 46 MB by keeping IDs double (or changing them to integer) and converting categorical variables to factor types. `-3.0`

- Q2 (76/80)

    - Q2.1 (10/10) Explain why read_csv cannot ingest labevents.csv.gz
    
    - Q2.2 (10/10) Explain why read_csv cannot ingest labevents.csv.gz
    
    - Q2.3 (15/15) The Bash code should be able to generate a file `labevents_filtered.csv.gz` (166MB). Check the numbers of rows and columns are correct.
    
      The number of rows we have is 32,679,896. Your result (32,651,024) is likely due to `$2 != "" && $7 != "" && $10 != ""` in your code, which is fine. However, this discrepancy perhaps affects future assignments (maybe it's fine).
  
    - Q2.4 (15/15)
    
    - Q2.5 (11/15)
    
      The binary Parquet file should be about 2.5 GB even though you have slightly smaller rows. `-4.0`
      
      When I used my `labevents.csv`, I got 2.5GB, which means your `file.info` code is correct. The difference is likely because you used the filtered file with `gzip -d -c labevents_filtered.csv.gz > labevents.csv`.
      
    - Q2.6 (15/15)

- Q3 (30/30) Steps should be documented and reproducible. Check final number of rows and columns.
	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw2 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put auxiliary and big data files into version control. 

All good.

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

All good.

### R code style: 20/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.

No violations.