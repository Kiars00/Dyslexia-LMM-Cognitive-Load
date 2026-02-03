# Dyslexia-LMM-Cognitive-Load

# Linear Mixed Models for Linguistic Processing in Dyslexia: A Cognitive Load Study

## 📌 Project Overview
This project is the culmination of my **Master’s in Clinical Linguistics**. It investigates the impact of gender bias and grammatical gender agreement on the linguistic processing of Italian adults with **Developmental Dyslexia (DG)** compared to a **Control Group (CG)**.

Using the **Grammaticality Maze task**, I measured reaction times (RTs) and accuracy to test how cognitive load is modulated by informational incongruence.

## 🔬 Statistical Approach: Linear Mixed Models (LMM)
The core of this research is the application of **Linear Mixed-Effects Models** to account for the nested structure of psycholinguistic data.
- **Random Effects:** Crossed random effects for *Participants* (ID) and *Items* (ID frase), allowing for a more robust estimation than traditional ANOVA.
- **Fixed Effects:** Interaction between *Group* (CG vs DG), *Grammatical Gender*, and *Gender Bias*.
- **Accuracy Analysis:** Implementation of **Generalized Linear Mixed Models (GLMM)** with a binomial distribution (logit link) to analyze correctness rates.

## 🛠️ Data Pipeline
The analysis follows a rigorous three-step pipeline:

1.  **Preprocessing (Python/Pandas):** - Cleaning raw PCIbex outputs.
    - Adaptive outlier removal using a participant-specific Standard Deviation method (±2.5 SD).
    - Data merging and anonymization (GDPR compliance).
2.  **Exploratory Data Analysis (R/tidyverse):** - Demographic characterization and T-tests for age/gender matching.
    - Visualization of RT slopes across sentence regions (Determiner, Noun, Verb, Pronoun).
3.  **Inferential Statistics (R/lme4):** - Competitive model testing (Null vs. Main Effects vs. Interaction models).
    - Model selection via **AIC** and **Likelihood Ratio Tests (ANOVA)**.

## 📂 Key Visualizations
Included in the `/plots` folder:
- **RT Slopes:** Comparison of processing costs across sentence regions.
- **Interaction Plots:** Visualizing the selective "sequestration" of executive resources in dyslexic readers during mismatched conditions.
- **Accuracy Distributions:** Highlighting the "floor effect" and systemic collapse in specific clinical phenotypes.

## 🛠️ Technologies
- **Python 3.13.2:** (Pandas, Regex) for data engineering.
- **R:** (lme4, lmerTest, sjPlot, tidyverse) for advanced statistical modeling.

---
**Author:** Chiara Mancuso  
**Tutor:** Prof.ssa Gloria Gagliardi  
**University of Bologna**
