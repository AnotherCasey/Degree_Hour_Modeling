# Degree_Hour_Modeling
A reproducible R workflow for calculating cumulative degree hours with modifiable temperature thresholds for mean hourly water temperature measurements aggregated from ten minute intervals. 

## Features

- Calculate cumulative degree hours using customizable temperature thresholds
- Support multiple monitoring sites and years
- Process continuous 10-minute water temperature observations pulled from HOBO loggers
- Aggregate observations to hourly mean temperatures
- Generate seasonal and annual degree-hour summaries
- Produce publication-quality visualizations
- Modular preprocessing workflow with documented quality control procedures

- 
## Workflow

```text
Raw 10-minute Temperature Data
               │
               ▼
Quality Control
               │
               ▼
Gap Filling (Harmonic Interpolation)
               │
               ▼
Hourly Aggregation
               │
               ▼
Degree Hour Calculation
               │
               ▼
Plots and Summary Tables
```

---
## Requirements

- R 

Primary packages include:

- dplyr
- lubridate
- tidyr
- ggplot2
- readr
- gt

## Future Development

Potential future additions include:

- Spatial interpolation of degree-hour surfaces
- Additional visualization tools
- Automated reporting
- Support for additional environmental variables
