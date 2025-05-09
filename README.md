
# heart disease analysis

Dataset is from hospital admissions reporting chest pain & amalgamates 5 different datasets. 1190 total observations, 746 final observations used in analysis ommitting duplications and NA counts. CSV file. 

## how/what 

Using PCA, logistic regression/ANCOVA and AIC to determine 1) if age and sex can predict exercise-induced angina, 2) if cholesterol levels and resting ECG results can be predictive of an exercise-induced angina, and 3) if the occurence of an exercise induced angina predicts heart disease. R FILE. 

### dependencies

* Packages used: ggplot2, tidyverse, janitor, factoextra, vegan, car, AICcmodavg, emmeans
* Works better on RStudio than Base R
* Residual plots must be manually processed (prompt to view all 4 must be run) before next lines are run  

## authors

5 person group project. Original PCA ANCOVA and AIC not mine - each created independently by respective group members. I integrated and compiled analyses into a cohesive & functional pipeline to resolve compatability/syntax issues and ensuring script functionality as a unit; troubleshooted & standardized code syntax for consistency and better integration/function as a coordinated analyses rather than seperate statistical methods 
