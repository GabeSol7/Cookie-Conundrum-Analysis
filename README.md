# **Cookie Conundrum Analysis**

### **An Investigation of Consumer Behavior and Online Privacy Regulations**
**Author**: Gabriel Solis (solisgab@usc.edu)  
**Advised by**: Alex P. Miller, Assistant Professor of Quantitative Marketing, USC  

---

## **Overview**
This repository contains the materials and analysis for my sophomore year research project, **"The Cookie Conundrum: An Investigation of Consumer Behavior and Online Privacy Regulations."** The project explores the unintended consequences of consent-based privacy mechanisms (e.g., GDPR and CCPA) and examines how firm characteristics like size and legacy influence consumer decisions about online data sharing.

The findings suggest that consumers exhibit higher trust in large, established firms over smaller start-ups, potentially creating unintended barriers to competition and innovation. The repository includes the complete analysis, the final research paper, and the dataset used in the study.

---

## **Repository Structure**
The repository is organized into the following directories:

1. **`data/`**  
   - `cookies_survey.csv`: The raw survey dataset collected during the project.  
   - `cookies_clean.csv`: The cleaned version of the dataset after quality checks and preprocessing.  

2. **`analysis/`**  
   - `Cookies_Analysis_Coding_Sample.R`: The complete R script used to clean the data, run statistical analyses, and generate visualizations.  
   - `output/`: Contains generated outputs, including visualizations and processed datasets.  

3. **`paper/`**  
   - `GabrielSolis_WritingSample.pdf`: The full research paper detailing the experiment, methodology, results, and implications.

---

## **Requirements**
To reproduce the analysis or run the scripts in this repository, you will need the following:

### **Software:**
- R (v4.0 or higher)

### **R Packages:**
Ensure the following R packages are installed:
- `tidyverse` (data manipulation and visualization)
- `GGally` (enhanced visualization)
- `reshape2` (data reshaping)
- `cowplot` (plot arrangement)
- `infer` (statistical inference tools)
- `lmtest` (regression diagnostics)
- `scales` (for formatting scales in ggplot2)

You can install these packages using the command:
```R
install.packages(c("tidyverse", "GGally", "reshape2", "cowplot", "infer", "lmtest", "scales"))
```

---

## **How to Use This Repository**
1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/cookie-conundrum.git
   cd cookie-conundrum
   ```

2. Navigate to the **`analysis/`** directory and open the R script (`Cookies_Analysis_Coding_Sample.R`) in your R environment.

3. Ensure the raw data file (`cookies_survey.csv`) is placed in the `data/` directory.

4. Run the R script to replicate:
   - Data cleaning and preprocessing steps
   - Statistical hypothesis testing
   - Visualizations of survey results and experimental findings
   - Output generation for cleaned data and analysis summaries

5. Refer to the `paper/` directory to read the final research paper (`GabrielSolis_WritingSample.pdf`) for detailed insights into the study.

---

## **Project Highlights**
- **Purpose**: This study investigates how privacy regulations influence competition by examining consumer trust and consent behavior.  
- **Key Findings**:  
  - Consumers are more likely to trust large, established firms over smaller start-ups when accepting data-tracking cookies.  
  - Current consent mechanisms may unintentionally favor market incumbents, reducing competition.  

- **Methodology**:  
  - Conducted a randomized experiment with 153 participants.  
  - Combined survey responses with statistical and visual analyses to derive insights.  

- **Implications**: Privacy regulations aimed at empowering consumers may unintentionally disadvantage smaller firms, potentially increasing market concentration.

---

## **Contact**
For questions or feedback, please feel free to reach out to me via email: **solisgab@usc.edu**

---

## **Acknowledgments**
I would like to thank Alex P. Miller for his guidance throughout this project and the USC community for their support.
