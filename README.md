# sia.project.1.wi.data

Data-wrangling pipeline to build **Shiny-ready** and **OSF-ready** datasets for the Wearables Inventory (WI).  
The workflow follows best practices from _[Reproducible Science in Ecology & Evolution](https://ecorepsci.github.io/reproducible-science/)_—emphasising tidy data, version control, scripted processing, and frozen environments.

---

## Why this repo?
- One canonical, scripted pipeline from **raw Excel inputs** → **tidy mother tables** → **one-row-per-device view** → **exports for Shiny & OSF**.
- Deterministic runs, minimal manual steps, clear provenance of each output.

---

## Repository structure

```bash
sia.project.1.wi.data/
├─ data/
│  ├─ raw/                # Excel inputs (not tracked, see .gitignore)
│  └─ output/             # Pipeline outputs (CSV/XLSX) for Shiny & OSF
├─ R/
│  ├─ build_df_shiny_wi.R # Main script: reads Excels, validates, exports
│  └─ utils.R             # Small helpers (yn_to_logical, norm_id, etc.)
├─ renv/                  # Reproducible R environment (optional)
├─ .gitignore
└─ README.md
