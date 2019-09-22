# rElki
R API to Java Data Mining Framework ELKI (https://elki-project.github.io/)

## Summary
The goal of this project is to offer an R interface to the Java data mining framework ELKI. ELKI provides a large collection of highly parameterizable algorithms, with an emphasis on unsupervised methods in cluster analysis and outlier detection. For more information on ELKI, see [here](https://elki-project.github.io/). This repository contains the ELKI jar [version 0.75](https://elki-project.github.io/releases/release_notes_0.7.5). If using this repository for research, please cite ELKI as explained [here](https://elki-project.github.io/publications) to give credit.

Currently, this interface implements twelve algorithms from the area of outlier detection. In case there's interest in using more of ELKI's algorithms from R, feel free to reach out!

## How To Use
The easiest way to use rElki in your R project is via [devtools](https://github.com/r-lib/devtools). To install devtools from CRAN, run `install.packages("devtools")` in your R environment.

You can now use devtools' `install_github` function to install the `rElki` repository in your environment. Run `devtools::install_github("lenaWitterauf/rElki")` to get the latest master version from this repository. In order to run rElki, you'll also need to install the R library [rJava](https://cran.r-project.org/web/packages/rJava/index.html) as well as a current version of Java on your computer, and then you're ready to go!

### Example - Outlier Scores for CSV Data
For this example let's assume we have a CSV file called `some_data.csv` containing data points we want to calculate outliers for. 
1. Load rElki - this will initialize the JVM
  ```R
  library(rElki)
  ```  
2. Read the data from `some_data.csv` into a dataframe
  ```R
  my_df <- read.csv("some_data.csv")
  ```  
3. Run an outlier detection from rElki. For this example, let's use Fast ABOD with a neighbourhood size of `3`
  ```R
  my_outlier_scores <- rElki::fast_abod(my_df, 3)
  ```  
4. Print each observation in the dataset along with its outlier score
  ```R
 for(index in c(1:nrow(my_df))) {
     print(paste('Observation:', paste(my_df[index,], collapse=',')))
     print(paste('Score:',       my_outlier_scores[index]))
 }
  ```  
  
### Example - Outlier Scores for Random Normal Distributed Data
1. Load rElki - this will initialize the JVM
  ```R
  library(rElki)
  ```  
2. Generate normally distributed data points
  ```R
  my_df <- replicate(5, rnorm(20))
  ```  
3. Run an outlier detection from rElki. For this example, let's use ODIN with a neighbourhood size of `2`
  ```R
  my_outlier_scores <- rElki::odin(my_df, 2)
  ```  
4. Print each observation in the dataset along with its outlier score
  ```R
 for(index in c(1:nrow(my_df))) {
     print(paste('Observation:', paste(my_df[index,], collapse=',')))
     print(paste('Score:',       my_outlier_scores[index]))
 }
  ```  

## Licence
This project, as the ELKI source code, is licenced under GNU AGPL 3. The ELKI jar in this repository includes parts that are licensed using different terms, such as the Apache License, BSD licenses or also public domain. It remains your responsibility to verify the license status when using or redistributing any of the files included. Using any of these files is WITHOUT ANY WARRANTY.
