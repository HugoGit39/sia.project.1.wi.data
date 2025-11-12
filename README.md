# sia.project.1.wi.data

Data-wrangling pipeline to build **Shiny-ready** and **OSF-ready** datasets for the Wearables Inventory (WI).  
The workflow follows best practices from _[Reproducible Science in Ecology & Evolution](https://ecorepsci.github.io/reproducible-science/)_ — emphasising tidy data, version control, scripted processing, and reproducible environments.

---

## Why this repo?

- Provides a **reproducible data pipeline** from raw Excel files to Shiny/OSF-ready datasets.  
- Ensures **tidy, relational** structures with `device_id` as the primary key.  
- Guarantees transparency, traceability, and consistency between analyses and published data.

---

## Repository structure

```bash
sia.project.1.wi.data/
├─ data/
│  ├─ processed/           
│  ├─ raw/                
│  └─ output/
│     ├─ data/             
│     ├─ plots/            
│     └─ reporting/        
├─ src/
│  ├─ application/         
│  ├─ function/            
│  ├─ reporting/           
│  └─ temp/               
├─ .gitignore
├─ sia.project.1.wi.data.Rproj
└─ README.md
```

## Inputs (expected in `data/raw/`)

| File | Description |
|------|--------------|
| **devices.xlsx** | Core device metadata (manufacturer, model, website, release date, market status, main use, etc.) |
| **signals.xlsx** | Long format: includes `sampling_rate_min/max`, `additional_info`, and `recording_location` |
| **technical_specs.xlsx** | Device specifications (battery life, connectivity, etc.) |
| **data_access.xlsx** | Data storage type, raw data access, SDK/API availability |
| **rvu_synthesis.xlsx** | Validity, reliability, and usability summaries |
| **expert_scores.xlsx** | Expert-based scoring (e.g., short-term, long-term) |

---

## Outputs (written to `data/output/data/`)

| File | Description |
|------|--------------|
| **df_shiny_wi.csv / df_shiny_wi.xlsx** | Dataframe for Shiny
| **df_osf_wi.csv / df_osf_wi.xlsx** | Dataframe for OSF


