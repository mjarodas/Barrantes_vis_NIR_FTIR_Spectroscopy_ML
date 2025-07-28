# Detection and Discrimination of Barrantes wine adulteration: A Machine Learning Approach Using vis-NIR and FTIR Spectroscopic Data
# 📌 Overview
This repository contains the code and workflows for the detection, discrimination and quantification of adulteration of Barrantes wine with other red wines using visible and near-infrared spectroscopy (vis-NIRS) and fourier transformation infrared spectroscopy (FTIR) in combination with machine learning techniques.
The aim of this research is the evaluation of different classification models to detect the presence of different red wines in Barrantes wine and regression models for the quantification of the same in percentages from 10% to 60%.
Article avaiable: https://doi.org/10.1016/j.saa.2025.126723

# 📂 Project Structure

The repository is structured as follows:

├── figures/                             # Generated figures from data analysis

├── scripts/                             # Contains scripts for data analysis

│   ├── 1. Exploratory                              # Exploratory Data Analysis (EDA)

│       ├── EDA_script.R│       

│   ├── 2. Spectra                              # vis_NIR spectra ploting 

│       ├── Spectra_visualization_script.R

│   ├── 3. Unsupervised_algorithms          # Unsupervised Machine Learning (HCA, PCA)

│       ├── HCA_script.R

│       ├── PCA_script.R

│   ├── 4. Supervised_classification_algorithms            # Supervised Machine Learning (SVM, RF and PLS-DA)

│       ├── pretreatments_script.R              #Aplication of Normalization to maximum, first and second derivative, Savitzky-Golay filter, MSC and SNV pretreatments

│       ├── SVM_Cross Validation_script.R      #Evaluation of SVM-cross validation for clasiffication according to the adulterant

│       ├── SVM_Train_Test_script.R      #Evaluation of SVM-train (70%) test (30%) for clasiffication according to the adulterant

│       ├── RF_Cross Validation_script.R      #Evaluation of Random Forest-cross validation for clasiffication according to the adulterant

│       ├── RF_Train_Test_script.R      #Evaluation of Random Forest-train (70%) test (30%) for clasiffication according to the adulterant

│       ├── PLS_Cross Validation_script.R      #Evaluation of PLS-DA-cross validation for clasiffication according to the adulterant

│       ├── PLS_Train_Test_script.R      #Evaluation of PLS-DA-train (70%) test (30%) for clasiffication according to the adulterant

│   ├── 4. Supervised_Regression_algorithms            # Supervised Machine Learning for regression (PLS, SVR, and RF)

│       ├── Regression_script.R      

├── requirements.txt                     # Required R packages

├── README.md                            # Project documentation

├── LICENSE                              # License file


# 🔄 Workflow
The data analysis workflow follows these main steps:

- Exploratory Data Analysis (EDA): Detection of missing values and outliers

- Spectra Visualization based on pure red wine adulterants and Barrantes wine

- Exploratory assessment of the dataset using Hierarchical Clustering Analysis (HCA) and Principal Component Analysis (PCA)

- Supervised Machine Learning Classification

Application of different pre-treatments: raw data, maximum normalisation, first and second derivative, Savitzky-Golay filter, Multiplicative Scatter Correction and Standrad Normal Variate pretreatments.

Evaluation of different classification models: Support Vector Machine, Random Forest and PLS-DA for classification according to the presence or not of adulterant and its nature. 

Two types of analysis have been used: cross validation and training-test analysis (70%:30%).

- Supervised Machine Learning Regression
Once a model has been developed for classification according to the presence or absence of an adulterant and its nature, the machine learning regression methods of SVR, PLS, and RF are evaluated for the quantification of adulteration for each adulterant.


# 🖥️ Software and Dependencies
The analysis is conducted in R (v4.4.0). The required R packages are specified in requirements.txt.

# 📜 License
This project is licensed under the GNU GENERAL PUBLIC License. See LICENSE for deta
