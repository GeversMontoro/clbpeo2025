# CLBPEO 2025 Analysis Repository

This repository contains the complete statistical analysis pipeline for the Chronic Low Back Pain Exercise Online (CLBPEO) 2025 study. The analysis is organized into three main folders corresponding to different analytical approaches.

## Repository Structure

### 📁 Primary_analysis/
Contains the core intention-to-treat (ITT) analyses and missing data handling:

- **`1.CLBPEO_ITT.R`** - Main ITT analysis with mixed-effects models for pain outcomes
- **`1.CLBPEO_MCAR_imputation.R`** - Multiple imputation analysis using MICE (PMN method) and MCAR testing
- **`1.CLBPEO_PP.R`** - Per-protocol analysis
- **`1b.CLBPEO_vs_CLBPE.R`** - Comparison between CLBPEO and CLBPE studies

### 📁 Secondary_outcomes/
Contains analyses for secondary outcome measures:

- **`2.CLBPEO_PIS.R`** - Pain Intensity Scale analysis
- **`2.CLBPEO_Questionnaires.R`** - Analysis of questionnaire-based outcomes
- **`2.CLBPEO_Questionnaires_by_group.R`** - Questionnaire analysis by group (Videos vs Zoom)
- **`2.CLBPEO_Physical.R`** - Analysis of physical performance measures
- **`2.CLBPEO_Physical_by_group.R`** - Physical measures analysis by group
- **`2.CLBPEO_MeP.R`** - Movement-evoked pain analysis
- **`2.CLBPEO_MeP_by_group.R`** - Movement-evoked pain analysis by group

### 📁 Moderation_Mediation/
Contains moderation and mediation analyses:

- **`3.CLBPEO_Moderators.R`** - Moderation analysis examining treatment effect modifiers
- **`3.CLBPEO_mlmed.R`** - Main mediation analysis for PCS and Chairst30
- **`3.CLBPEO_mlmed_by_group.R`** - Group-based mediation analysis (Videos vs Zoom)
- **`3.CLBPEO_mlmed_othervar.R`** - Mediation analysis for 22 additional potential mediators

## Key Features

### 🔬 Statistical Methods
- **Mixed-Effects Models (LMM)** using `lme` function
- **Multilevel Mediation Analysis** using `multilevelmediation` package
- **Bootstrap Resampling** (5000 iterations) for confidence intervals and p-values
- **Multiple Imputation** using MICE with PMN method
- **MCAR Testing** using Little's test

### 📊 Main Findings
- **Primary Mediators**: PCS (Pain Catastrophizing Scale) and Chairst30 (30-second chair stand test)
- **Significant Indirect Effects**: Both PCS and Chairst30 showed significant mediation effects
- **Group Differences**: No significant differences between Videos vs Zoom delivery methods
- **Other Mediators**: 22 additional variables tested, none showed significant indirect effects

### 🔧 Technical Details
- **Reproducibility**: All bootstrap analyses use `set.seed(1234)`
- **Convergence Handling**: Robust error handling for singular fits and convergence issues
- **Output Management**: Comprehensive results saved to text files
- **Data Transformation**: Special handling for EDD (log transformation) and other variables

## Usage

### Prerequisites
```r
# Required R packages
install.packages(c("readxl", "lme4", "lmerTest", "ggplot2", "dplyr", 
                   "performance", "devtools", "multilevelmediation", 
                   "tidyr", "nlme", "MCMCpack", "boot", "naniar", 
                   "mice", "BaylorEdPsych", "tidyverse", "ggrain", 
                   "patchwork"))

# Install multilevelmediation from GitHub if needed
devtools::install_github("falkcarl/multilevelmediation")
```

### Running Analyses
```bash
# Primary analysis
Rscript 1.CLBPEO_ITT.R

# Secondary outcomes
Rscript 2.CLBPEO_questionnaires.R

# Mediation analysis
Rscript 3.CLBPEO_mlmed.R
```

## Output Files

- **`model_mlmed_main.txt`** - Main mediation results (PCS and Chairst30)
- **`model_mlmed_by_group.txt`** - Group-based mediation results
- **`model_mlmed_other_mediators.txt`** - Results for 22 additional mediators
- **`model_moderators.txt`** - Moderation analysis results

## Statistical Notes

### Singularity Issues
Some models show singularity warnings due to:
- Low variance in certain mediators
- Missing data patterns
- Model specification challenges

These are handled robustly with error catching and alternative estimation methods.

### Bootstrap Analysis
- **5000 iterations** for all bootstrap analyses
- **Seed 1234** ensures reproducibility
- **Confidence intervals** calculated using percentile method
- **P-values** derived from bootstrap distributions

## Citation

If you use this analysis pipeline, please cite:

```
Gevers-Montoro, C. (2025). CLBPEO 2025 Analysis Repository. 
Chronic Low Back Pain Exercise Online Study Statistical Analysis Pipeline.
```

## Contact

For questions about the analysis or repository, please contact the corresponding author.

---

**Note**: This repository contains the complete statistical analysis pipeline for the CLBPEO 2025 study. All analyses are reproducible and include comprehensive error handling for robust statistical inference.
