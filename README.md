# Justice Delayed, Justice Denied: Statistical Bail Analysis

## Overview
This project is a rigorous statistical investigation into bail processing times across the Indian judicial system. Utilizing the DAKSH Legal Database, we analyzed 9,870 bail cases to test the core hypothesis: **Anticipatory bail applications take significantly longer to resolve than Regular bail applications.**

This repository contains the R scripts used for the statistical analysis, the dataset, and the source code for the interactive web presentation of our findings.

## Core Hypothesis & Methodology
To handle the extreme right-skew and multi-year outliers in the judicial data, the analysis was conducted in four escalating phases:

1. **Normality Diagnostics:** Kolmogorov-Smirnov test confirmed non-normal distribution with extreme right-skewness.
2. **Non-Parametric Testing:** Mann-Whitney U test compared rank distributions, revealing a highly significant difference in processing times.
3. **Parametric Trap Identification:** Demonstrated how log-transformations and T-Tests introduce survivorship bias by forcing the exclusion of censored (pending) cases.
4. **Survival Analysis:** Kaplan-Meier survival curves and Log-Rank tests were applied to handle right-censored data accurately.

## Key Findings
* **Processing Delays:** Anticipatory bail takes 65% longer than Regular bail (Median of 33 days vs. 20 days).
* **The Hearing Trap:** A moderate positive correlation (Spearman's rₛ = 0.351) shows that more hearings signal longer delays due to compounding adjournments.
* **Structural Backlog:** A Kruskal-Wallis test across filing years revealed extreme right-censoring in older cohorts (2010–2015), indicating an accumulating systemic backlog.

## Tech Stack
* **Statistical Analysis:** R (Non-parametric tests, Kaplan-Meier Survival Analysis)
* **Frontend Visualization:** HTML5, CSS3, Tailwind CSS

## Team Members
* Zaheem 
* Masum Abbas 
* Asfaar Maham Ghazi
