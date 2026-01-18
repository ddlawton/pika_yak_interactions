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

This study investigates the facilitative interactions between plateau pikas (Ochotona curzoniae) and domestic yaks on the Tibetan Plateau. Through field surveys and manipulative experiments, we demonstrate that pikas enhance yak growth performance by suppressing the poisonous forb Stellera chamaejasme, thereby improving forage quantity, quality, and foraging efficiency for yaks.

### Key Findings

- Pikas preferentially feed on and clip S. chamaejasme, reducing its cover
- Pika presence increases grass and sedge abundance (preferred yak forage)
- Yaks gain more weight in plots with pikas, particularly when S. chamaejasme is present
- Pika presence improves forage quality (crude protein) and foraging efficiency (bites per step)
- Effects are density-dependent: yak weight gain increases with active pika burrow density

## Repository Structure

```
pika_yak_interactions/
├── R/                                    # All analysis scripts
│   ├── 01_data_management.R              # Data cleaning and preparation
│   ├── 02_diet_selection.R               # Pika and yak diet selection analyses
│   ├── 03_experiment_plant_cover.R       # Plant cover response to treatments
│   ├── 04_experiment_foraging_efficiency.R # Yak foraging behavior analyses
│   ├── 05_experiment_forage_quality.R    # Forage nutrient content analyses
│   ├── 06_active_burrow_yak_gain_analysis.R # Density-dependence analyses
│   ├── manuscript_figure_creation/       # Scripts for final figures
│   └── statistical_reports/              # Quarto reports for website
├── data/
│   ├── raw/                              # Original field data
│   └── processed/                        # Cleaned data and model outputs
├── output/                               # Figures and diagnostic plots
├── renv/                                 # Package management
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

The analysis workflow consists of sequential R scripts:

1. `01_data_management.R` - Cleans raw data and prepares it for analysis
2. `02_diet_selection.R` - Analyzes pika and yak feeding preferences (2021 field survey)
3. `03_experiment_plant_cover.R` - Models plant cover responses to treatments
4. `04_experiment_foraging_efficiency.R` - Analyzes yak foraging behavior
5. `05_experiment_forage_quality.R` - Models forage nutrient content
6. `06_active_burrow_yak_gain_analysis.R` - Tests density-dependent relationships

Each script:
- Reads data from `data/processed/`
- Fits statistical models
- Saves model outputs and figures
- Exports results for visualization

Figures for the manuscript are created using scripts in `manuscript_figure_creation/`.

### Statistical Reports

Interactive HTML reports documenting all analyses are available at:

https://ddlawton.github.io/pika_yak_interactions

The reports include:
- Complete statistical methods
- Model summaries and diagnostics
- All figures and tables
- Posthoc comparisons

To rebuild the website locally:
```r
quarto::quarto_render()
```

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

Processed data files are included in `data/processed/`. Raw field data are available upon request.

## Citation

If you use data or code from this repository, please cite:

[Citation information will be added upon publication]

## License

MIT License - see LICENSE file for details
