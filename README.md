# Qualitative Health Analysis

Final project for the course **Qualitative Data Analysis**.

## Project Description

This project explores the prediction of how many different doctors a respondent visited in the past year, based on qualitative responses from the **National Poll on Healthy Aging (NPHA)** conducted in April 2017 in the United States.  
The dataset, available through the [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/936/national+poll+on+healthy+aging+(npha)), was filtered and transformed to support binary classification.

## 🧠 Methodology

- Logistic regression models were fitted using **R**
- Both **logit** and **cloglog** link functions were tested
- Model selection performed via backward stepwise regression (`step()`)
- Evaluation metrics included:
  - **F1-score**
  - **Matthews Correlation Coefficient (MCC)**
- Dataset split into training/testing sets using `createDataPartition()` from the `caret` package

## 📄 Report

The full PDF report (in English) is available here:  
📎 [Open report](Qualitative_Data_Analysis___Mateusz_Mglej.pdf)

---

**Author:** Mateusz Mglej




