# Density-dependent facilitation of livestock by small mammal ecosystem engineers

This repository contains all data, code, and analyses for the manuscript:

**"Density-dependent facilitation of livestock by small mammal ecosystem engineers"**

Zhiwei Zhong<sup>1,2,3</sup>, Bingbo Ni<sup>2</sup>, Douglas Lawton<sup>4</sup>, Xiaofei Li<sup>1</sup>, Xiaona Zheng<sup>1</sup>, Huakun Zhou<sup>3</sup>, Junhu Su<sup>5</sup>, Wenjin Li<sup>6</sup>, Zhenggang Guo<sup>6</sup>, Fujiang Hou<sup>6</sup>, Quanmin Dong<sup>7</sup>, Shikui Dong<sup>8</sup>, Christopher R. Dickman<sup>9</sup>, Jens-Christian Svenning<sup>10</sup>, Ying Gao<sup>1*</sup>, Zhibin Zhang<sup>11,12*</sup>

<sup>1</sup> Key Laboratory of Vegetation Ecology of the Ministry of Education, Songnen Grassland Ecosystem National Observation and Research Station, Northeast Normal University, Changchun, Jilin, China.  
<sup>2</sup> Northeast Institute of Geography and Agroecology, Chinese Academy of Sciences, Changchun, Jilin, China.  
<sup>3</sup> Key Laboratory of Cold Regions Restoration Ecology, Northwest Institute of Plateau Biology, Chinese Academy of Sciences, Xining 810008, China.  
<sup>4</sup> School of Sustainability, Arizona State University, Tempe, AZ, USA  
<sup>5</sup> College of Grassland Science, Key Laboratory of Grassland Ecosystem (Ministry of Education), Gansu Agricultural University, Lanzhou, China  
<sup>6</sup> State Key Laboratory of Grassland Agro-Ecosystems, College of Pastoral Agriculture Science and Technology, Lanzhou University, Lanzhou City 730020, PR China  
<sup>7</sup> State Key Laboratory of Plateau Ecology and Agriculture, Qinghai Academy of Animal and Veterinary Science, Qinghai University, Xining 810016, China  
<sup>8</sup> School of Grassland Science, Beijing Forestry University, Beijing, 100083, China  
<sup>9</sup> School of Life and Environmental Sciences, The University of Sydney, Sydney, NSW 2006, Australia  
<sup>10</sup> Department of Biology, Center for Ecological Dynamics in a Novel Biosphere, Aarhus University, Aarhus DK-8000, Denmark  
<sup>11</sup> School of Ecology, Hainan International One Health Institute, Hainan Province Key Laboratory of One Health, Hainan University, Haikou, Hainan 570228, China  
<sup>12</sup> State Key Laboratory of Integrated Management of Pest Insects and Rodents, Institute of Zoology, Chinese Academy of Sciences, 1 Beichen West Road, Chaoyang District, Beijing 100101, China

## Overview

This study investigates the facilitative interactions between plateau pikas (*Ochotona curzoniae*) and domestic yaks on the Tibetan Plateau. Through field surveys and manipulative experiments, we demonstrate that pikas enhance yak growth performance by suppressing the poisonous forb *Stellera chamaejasme*, thereby improving forage quantity, quality, and foraging efficiency for yaks.

### Key Findings

- Pikas preferentially feed on and clip *S. chamaejasme*, reducing its cover
- Pika presence increases grass and sedge abundance (preferred yak forage)
- Yaks gain more weight in plots with pikas, particularly when *S. chamaejasme* is present
- Pika presence improves forage quality (crude protein) and foraging efficiency (bites per step)
- Effects are density-dependent: yak weight gain increases with active pika burrow density

## Repository Structure

```
pika_yak_interactions/
├── _targets.R                            # Targets pipeline orchestration
├── R/
│   ├── functions/                        # Modular analysis functions
│   │   ├── data_preprocessing.R          # Data cleaning utilities
│   │   ├── model_fitting.R               # Statistical model functions
│   │   ├── model_postprocessing.R        # Posthoc tests and predictions
│   │   └── visualization.R               # Plotting helpers
│   ├── targets/                          # Pipeline target definitions
│   │   ├── data_targets.R                # Data loading and preprocessing
│   │   ├── model_targets.R               # Model fitting targets
│   │   └── output_targets.R              # Output generation targets
│   └── statistical_reports/              # Quarto documents
│       └── manuscript_figures_and_text.qmd # Methods, results, figures, tables
├── data/
│   ├── raw/                              # Original field data (Excel files)
│   ├── clean/                            # Processed data (CSV)
│   │   ├── diet_selection/               # 2021 survey data
│   │   ├── plant_cover/                  # Plant community data
│   │   ├── foraging_efficiency/          # Yak behavior data
│   │   ├── forage_quality/               # Nutrient analysis data
│   │   └── additional_data/              # Burrow counts, weight gain
│   └── models/                           # Fitted model objects (.qs)
├── output/                               # Generated figures and diagnostics
│   ├── figures/                          # Publication-quality figures
│   └── diagnostics/                      # Model diagnostic plots
├── _targets/                             # Targets cache (auto-generated)
├── _site/                                # Rendered Quarto website
├── renv/                                 # Package environment management
├── _quarto.yml                           # Website configuration
└── README.md                             # This file
```

## Reproducibility

### Installation

1. Clone the repository:
```bash
git clone https://github.com/ddlawton/pika_yak_interactions.git
cd pika_yak_interactions
```

2. Install R (version 4.3.1 or later) and RStudio

3. Restore the R package environment using `renv`:
```r
renv::restore()
```

This will install all required packages at the versions used for the analyses.

### Running Analyses

The analysis workflow is managed by the [`targets`](https://books.ropensci.org/targets/) package, which ensures reproducibility and only re-runs necessary computations when dependencies change.

**To run the complete pipeline:**

```r
# Load the targets package
library(targets)

# Run the entire analysis pipeline
tar_make()
```

The pipeline:
1. **Data preprocessing** (`R/targets/data_targets.R`)
   - Loads and cleans raw Excel files
   - Creates analysis-ready CSV files in `data/clean/`

2. **Model fitting** (`R/targets/model_targets.R`)
   - Fits all statistical models (GLMMs, GAMs)
   - Saves model objects to `data/models/`

3. **Postprocessing** (`R/targets/output_targets.R`)
   - Computes estimated marginal means
   - Performs posthoc comparisons
   - Generates model predictions
   - Creates diagnostic plots

4. **Website generation**
   - Quarto renders `R/statistical_reports/manuscript_figures_and_text.qmd`
   - Produces interactive HTML reports with all figures and tables

**Useful targets commands:**
```r
# View the pipeline structure
tar_visnetwork()

# Check which targets are outdated
tar_outdated()

# Load a specific target into your R session
tar_load(model_weight_gain)

# Read a specific target (without loading dependencies)
tar_read(postprocessed_weight_gain)
```

### Statistical Reports

Interactive HTML reports documenting all analyses are available at:

https://ddlawton.github.io/pika_yak_interactions

The reports include:
- Complete statistical methods
- Model summaries with coefficients and significance tests
- All publication-quality figures
- Comprehensive supplementary tables
- Posthoc comparisons with adjusted p-values

**To rebuild the website locally:**

1. First run the targets pipeline to generate data and models:
```r
targets::tar_make()
```

2. Then render the Quarto document:
```r
quarto::quarto_render()
```

Or from the terminal:
```bash
quarto preview .
```

The website is automatically rebuilt and deployed via GitHub Actions on every push to `main`.

## Statistical Methods

All analyses used generalized linear mixed models (GLMMs) or generalized additive models (GAMs) implemented in R. Models accounted for the hierarchical structure of the data with random effects for block, year, month, and (where appropriate) individual yak identity.

Key model structure for experimental data:
```r
response ~ pika_treatment * S_chamaejasme_treatment +
    (1 | block) + (1 | year) + (1 | month)
```

Statistical families (Gaussian, Beta, Tweedie) were selected based on data distribution and model diagnostics. Posthoc comparisons used Tukey HSD adjustments. See the online reports for detailed methods.

### Software

- R version 4.3.1
- `glmmTMB` for generalized linear mixed models
- `mgcv` for generalized additive models
- `emmeans` for estimated marginal means
- `DHARMa` for model diagnostics
- `tidyverse` for data manipulation and visualization

Complete package versions are in `renv.lock`.

## Data Availability

- **Raw data**: Original field data files are in `data/raw/` (Excel format)
- **Processed data**: Analysis-ready CSV files are in `data/clean/`, organized by analysis type
- **Model outputs**: Fitted model objects (`.qs` format) are in `data/models/`
- **Results**: Model summaries, predictions, and posthoc tests are saved alongside processed data

All data processing is fully scripted and reproducible via the targets pipeline.

## Citation

If you use data or code from this repository, please cite:

[Citation information will be added upon publication]

## License

MIT License - see LICENSE file for details
