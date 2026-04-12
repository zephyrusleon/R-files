# INT Swimming Start Manuscript Package

This repository contains the current analysis script, manuscript-ready outputs, derived statistical tables, and main figures for the integrative neuromuscular training study in adolescent swimmers.

## Contents

- `repeated_measures_anova_plan.R`
  - Main R script for data processing, mixed ANOVA, sensitivity analyses, table generation, and figure generation.
- `INT/update_manuscript_red.ps1`
  - PowerShell script used to refresh the red-marked manuscript copy from the generated outputs.
- `INT/repeated_measures_anova_report.docx`
  - Statistical analysis report.
- `INT/manuscript_副本.docx`
  - Updated manuscript working copy with red revisions.
- `INT/anova_outputs/`
  - Derived CSV and XLSX outputs used for manuscript tables and supplementary statistical reporting.
- `INT/figures/`
  - Main manuscript figures.
- `jssm-24-128.pdf`
  - Reference article used for figure and table formatting calibration.

## Notes

- This package includes derived outputs and manuscript assets, not the original raw participant spreadsheets.
- Main inferential framework: 2 x 2 mixed repeated-measures ANOVA.
- Within-group `Post-pre ES (d)` values were calculated as:
  - `(Post - Pre) / sqrt((SD_pre^2 + SD_post^2) / 2)`

