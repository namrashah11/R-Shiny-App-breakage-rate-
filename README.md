# 🪟 Window Manufacturing Decision Support System (DSS)

## 📘 Project Overview

This Shiny dashboard is a **Decision Support System (DSS)** designed for a **window manufacturing company** to monitor, predict, and optimize the **glass breakage rate** during production. It uses a combination of **descriptive**, **predictive**, and **prescriptive analytics** to help manufacturing engineers and decision-makers reduce waste, improve efficiency, and optimize machine settings such as **cut speed**.

---

## 🎯 Objectives

- 📊 **Descriptive Analytics**: Visualize summary statistics and key production metrics like breakage rate distribution and correlations with glass thickness, supplier, and cut speed.
- 📈 **Predictive Modeling**: Use regression modeling to estimate breakage rate based on inputs like cut speed, glass thickness, window size, and ambient temperature.
- 🧮 **Prescriptive Analytics**: Recommend the optimal cut speed that minimizes breakage under given conditions.

---

## 🛠️ Technologies Used

- **R**
- **Shiny / shinydashboard**
- **ggplot2** (visualization)
- **dplyr / tidyverse** (data manipulation)
- **DT** (interactive tables)
- **readxl** (Excel file reading)

---

## 🧾 Dataset

- 📁 **File**: `Window_Manufacturing.xlsx`
- **Fields**:
  - `Glass_Supplier`: Source of the glass
  - `Window_Type`: Type of manufactured window
  - `Window_Size`: Size of the window in inches
  - `Glass_thickness`: Thickness of the glass in inches
  - `Cut_speed`: Speed of cutting machine (m/min)
  - `Ambient_Temp`: Ambient temperature (°C)
  - `Breakage_Rate`: Target variable – rate of breakage during production

> 📝 _The dataset is synthetic, created for academic simulation._

---

## 📊 Dashboard Structure

### 1. Business Problem Framing
- Explains the objective of reducing breakage rate.
- Maps the business challenge to an analytics solution.

### 2. Descriptive Analytics
- Summary table by supplier and window type
- Histograms, scatterplots, boxplots, and bar charts:
  - Breakage distribution
  - Breakage vs. cut speed
  - Breakage by glass thickness and supplier

### 3. Predictive Analytics
- Linear regression with quadratic terms for cut speed
- Model evaluation using F1-score and RMSE
- Visual plot of actual vs predicted breakage rates

### 4. Prescriptive Analytics
- Predicts **optimal cut speed** using a regression model
- Recommends values based on user-defined inputs

---

## 🔍 Notable Features & Code Practices

- ✅ Data validation, missing value imputation
- 📈 Custom RMSE function
- 🧪 Reactive filtering and user input handling
- 🔁 For-loop to simulate multiple training scenarios
- 🧠 Feature engineering and regression tuning
- 📦 Use of `list()`, `class()`, `ifelse()` to meet programming criteria

---

## 🚀 Getting Started

### 1. Install required packages:
```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "DT", "readxl"))
