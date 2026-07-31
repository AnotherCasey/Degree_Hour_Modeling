# Get Tides - Match NOAA Water Level to Temperature Observations

This script retrieves verified NOAA water level observations and matches each temperature measurement to the nearest available tide observation. The resulting dataset can be used to investigate relationships between tidal stage, water level, and estuarine water temperature.

---

## Features

- Import verified NOAA water level observations
- Convert timestamps to a common time zone
- Match each temperature observation to the nearest tide measurement
- QC - Record the time difference between matched observations
- Export a merged dataset for downstream analysis and visualization

---

## Inputs

- Temperature observations (10-minute interval)
- NOAA verified water level observations

---

## Outputs

A merged dataset containing:

- Temperature observations
- Matched water level
- Tide timestamp
- Time difference between observations

---

## Applications

This workflow can be used to:

- Evaluate temperature variation across tidal cycles
- Compare QC flags with tidal conditions
- Visualize water level alongside continuous temperature measurements

---

## Notes

Although NOAA water level observations are recorded at a different sampling interval than the temperature sensors, each temperature measurement is matched to its nearest available tide observation to facilitate comparison between the two datasets.
