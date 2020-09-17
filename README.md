It is a house property dataset provided by California based real estate organisation. The Problem statement is that we had to predict whether this property is being sold or not depending on various factors available in the dataset. This was basically a classification supervised learning problem in which our target variable was binary classified. So I used the simplest machine learning algorithm which is used to solve binary classification supervised learning problem i.e "Logistic regression".

This project goes in the following manner :-

a) Firstly I imported my dataset in Rstudio. Had a brief look to summary and tried to understand the data.

b) I started doing Exploratory Data Analysis (EDA) which consisted checking the missing values if there then treated it, checking outlires by plotting the boxplots for different attributes if found any then treated it using caping method, visualize different attributes distribution by plotting multiple graphs and done univariate analysis, bivariate analysis and feature engineering. Finally prepared the data for modelling purpose.

c) Splitted the data into training set and test set to build the models.

d) Prepared multiple models with training test using glm function, predicted the model over test set and calculated multiple performance metric like accuracy, precision, recall and AUC value of ROC curve by creating confusion matrix.

e) My final model has accuracy of 85% and AUC value of ROC curve is 0.84.
