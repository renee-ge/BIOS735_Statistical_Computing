# BIOS735_Statistical_Computing

## What are the potential biomarkers of biological age?

This project aims to address this question. The concept of "biological age" reflects the extent to which an individual is impacted by aging-driven biological changes and can capture physiological deterioration better than chronological age. 
Finding markers of biological age would allow for more efficient implementation of health-related interventions. 

## Data

The dataset we used was taken from [Kaggle](https://www.kaggle.com/datasets/simaanjali/diabetes-classification-dataset).
The data set contains clinical data obtained from a cohort of 5,132 patients. Variables encompass
critical demographic and physiological including age (Age), gender (Gender), body mass index
(BMI), total cholesterol level (Chol), triglycerides level (TG), high-density lipoprotein level
(HDL), low-density lipoprotein level (LDL), creatinine level (Cr), blood urea nitrogen level
(BUN), and an indicator of having diabetes (Diagnosis). For the purposes of our project, age will
be the primary response variable for predictive modeling.

## Methods

We used Lasso Regression, Random Forests (RF), and Gradient Boosting Machines (GBM) to analyze the importance of features in the prediction of age.
We further compared the performance of the three methods. As part of the project parameters, we were required to code our own Lasso regression using 
coordinate descent and could not use pre-built function in any R packages. To get the feature importance scores in Lasso Regression analogous to the machine learning methods, weused bootstrap for statistical inference. 
500 bootstrap samples were generated from the training data and Lasso Regression was fitted on each sample to obtain the coefficients. 
The importance score of each feature was calculated as the proportion of times it was nonzero across all bootstrap samples. Features with higher importance scores, i.e. those more frequently selected, are considered more important in the Lasso regression. 
The lasso_bootstrap_inference function was used to perform the bootstrap inference. It returns the probability of each feature being selected in the Lasso model across the bootstrap samples as described above.
The caret R package was used to train the RF model and the GBM R package was used to train the GBM.

## Project Organization

* The folder Analysis contains the R code for all analyses in the project
  - EDA.Rmd contains all exploratory data analyses performed
  - Lasso_Regression.R is an R script that contains the self-coded Lasso Regression function
  - ml_function_v2.R is an R script that provides function for RF and GBM tuning 
  - lasso_prediction.R implements the Lasso_Regression function and performs the features selection and testing
* ml_prediction.R trains and tests the RF and GBM models in the data 
* report.Rmd is the Rmardown file that compiles the analyses and results into one final report and report.pdf is the readable version of this report
  
