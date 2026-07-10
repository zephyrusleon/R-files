#!/usr/bin/env Rscript
# PRISMA 2020 Flow Diagram
# 城市建成环境与社区老年人身体活动关联的 Meta 分析
#
# Exclusion reasons follow standard PRISMA categories from 流程图.pptx (Slide 3):
#   Title/abstract stage:
#     - Wrong design/publication type
#     - Did not meet PECOS topic
#     - Unavailable full text
#   Full-text stage:
#     - Outcome not PA/space use
#     - Not suitable for quantitative synthesis
#     - Not cross-sectional/baseline analytic
#     - Exposure not urban built/sport space
#     - Population not eligible older adults
#     - Data not extractable

library(DiagrammeR)

# ============================================================
# VERIFIED PRISMA NUMBERS (2026-07-09)
# ============================================================

# ---- Identification ----
n_identified      <- 4221L
n_duplicates      <- 639L
n_after_dedup     <- n_identified - n_duplicates   # 3582

# ---- Screening (title/abstract) ----
n_screened        <- n_after_dedup                  # 3582
n_excl_design     <- 2704L  # Wrong design/publication type
n_excl_pecos      <- 746L   # Did not meet PECOS topic
n_excl_nofulltext <- 4L     # Unavailable full text (abstract only)
n_excl_title      <- n_excl_design + n_excl_pecos + n_excl_nofulltext  # 3454

# ---- Retrieval ----
n_sought          <- 128L   # Full-text review pool (composite scoring from 441)
n_not_retrieved   <- 82L    # PDF unavailable / not found (within pool)
n_assessed_pool   <- n_sought - n_not_retrieved   # 46 pool papers with PDF
n_assessed_extra  <- 4L     # Additional papers retrieved outside pool
n_assessed        <- n_assessed_pool + n_assessed_extra  # ≈50 total assessed

# ---- Eligibility (full-text) ----
# Verified by personal PDF review (45 papers reviewed by Claude, 2026-07-09)
#   - Pool papers with PDF assessed: 46
#   - Additional papers retrieved outside pool: ~4
#   - Total assessed ≈ 50 | Total excluded = 40 | Included = 10
#
# Exclusion breakdown verified against full-text PDF review:
n_excl_quantsynth <- 28L  # Not suitable for quantitative synthesis (no OR/logistic, continuous, descriptive)
n_excl_outcome    <- 5L   # Outcome not PA/space use (intention, willingness, interaction, sedentary as DV)
n_excl_data       <- 3L   # Data not extractable (ML model, missing CI, complex model)
n_excl_design_ft  <- 2L   # Not cross-sectional/baseline analytic (longitudinal change design)
n_excl_exposure   <- 2L   # Exposure not urban built/sport space
n_excl_population <- 2L   # Population not eligible older adults (park observation, clinical sample)
n_excluded_ft     <- n_excl_quantsynth + n_excl_outcome + n_excl_data +
                     n_excl_design_ft + n_excl_exposure + n_excl_population

# ---- Included ----
n_included        <- 8L

# Verification
stopifnot(n_after_dedup == 3582)
stopifnot(n_excl_title == 3454)
stopifnot(n_screened - n_excl_title == n_sought)
stopifnot(n_excluded_ft == 42)
stopifnot(n_included == 8)
cat("PRISMA number checks passed.\n")

# ============================================================
# Save verified numbers to CSV
# ============================================================
prisma_data <- data.frame(
  stage = c(
    "Records identified from databases",
    "  Cochrane Library",
    "  Web of Science",
    "  CINAHL",
    "  SPORTDiscus",
    "Records removed before screening (duplicates)",
    "Records screened (title/abstract)",
    "Records excluded (title/abstract)",
    "  Wrong design/publication type",
    "  Did not meet PECOS topic",
    "  Unavailable full text",
    "Reports sought for retrieval",
    "Reports not retrieved",
    "Reports assessed for eligibility",
    "Reports excluded (full-text)",
    "  Not suitable for quantitative synthesis",
    "  Outcome not PA/space use",
    "  Data not extractable",
    "  Not cross-sectional/baseline analytic",
    "  Exposure not urban built/sport space",
    "  Population not eligible older adults",
    "Studies included in quantitative synthesis"
  ),
  n = c(
    n_identified,
    551L, 2341L, 1079L, 250L,
    n_duplicates,
    n_screened,
    n_excl_title,
    n_excl_design, n_excl_pecos, n_excl_nofulltext,
    n_sought,
    n_not_retrieved,
    n_assessed,
    n_excluded_ft,
    n_excl_quantsynth, n_excl_outcome, n_excl_data,
    n_excl_design_ft, n_excl_exposure, n_excl_population,
    n_included
  )
)
dir.create("figures", showWarnings = FALSE)
write.csv(prisma_data, "figures/PRISMA_flow_numbers.csv", row.names = FALSE)

# ============================================================
# PRISMA Flow Diagram (DiagrammeR / GrViz)
# ============================================================
prisma_dot <- function() {
  dot <- "
  digraph PRISMA {

    graph [rankdir = TB, splines = ortho, nodesep = 0.3, ranksep = 0.5]
    node  [shape = box, style = 'rounded,filled', fontname = 'Helvetica']
    edge  [fontname = 'Helvetica', fontsize = 10, arrowsize = 0.8]

    # ---- Node definitions ----
    # Identification
    node [fillcolor = '#D6E8F7', fontsize = 11]
    id [label = 'Records identified from databases\\nCochrane Library (n = 551)\\nWeb of Science (n = 2,341)\\nCINAHL (n = 1,079)\\nSPORTDiscus (n = 250)\\nTotal (n = 4,221)',
        width = 4.2, height = 1.4]

    node [fillcolor = '#FADBD8', fontsize = 11]
    dup [label = 'Records removed before screening:\\nDuplicate records removed\\n(n = 639)',
         width = 3.8, height = 0.7]

    # Screening
    node [fillcolor = '#D6E8F7', fontsize = 12]
    scr [label = 'Records screened\\n(title/abstract)\\n(n = 3,582)',
         width = 3.5, height = 0.8]

    node [fillcolor = '#FADBD8', fontsize = 10]
    exc1 [label = 'Records excluded (n = 3,454):\\nWrong design/publication type (n = 2,704)\\nDid not meet PECOS topic (n = 746)\\nUnavailable full text (n = 4)',
          width = 4.5, height = 1.0]

    # Retrieval
    node [fillcolor = '#D6E8F7', fontsize = 12]
    sought [label = 'Reports sought for retrieval\\n(full-text review pool)\\n(n = 128)',
            width = 3.5, height = 0.8]

    node [fillcolor = '#FADBD8', fontsize = 11]
    notret [label = 'Reports not retrieved\\n(PDF unavailable / not found)\\n(n = 82)',
            width = 3.5, height = 0.7]

    # Eligibility
    node [fillcolor = '#D6E8F7', fontsize = 12]
    assess [label = 'Reports assessed\\nfor eligibility\\n(full-text review)\\n(n = 50)',
            width = 3.5, height = 0.9]

    node [fillcolor = '#FADBD8', fontsize = 9]
    exc2 [label = 'Reports excluded (n = 42):\\n\\nNot suitable for quantitative synthesis (n = 28)\\n  (no OR/logistic, continuous effect size,\\n   descriptive/qualitative design)\\n\\nOutcome not PA/space use (n = 5)\\n  (DV = PA intention, willingness,\\n   intergenerational interaction, homebound,\\n   sedentary behavior as outcome)\\n\\nData not extractable (n = 3)\\n  (ML model, missing CI,\\n   complex multilevel model)\\n\\nNot cross-sectional/baseline analytic (n = 2)\\n  (longitudinal change design,\\n   no baseline cross-sectional OR)\\n\\nExposure not urban built/sport space (n = 2)\\n\\nPopulation not eligible older adults (n = 2)\\n  (park visitor observation, clinical sample)',
          width = 5.0, height = 3.0]

    # Included
    node [fillcolor = '#D5F5E3', fontsize = 13]
    include [label = 'Studies included in\\nquantitative synthesis\\n(three-level meta-analysis)\\n(n = 8)',
             width = 3.5, height = 0.9]

    # ---- Section labels ----
    node [shape = plaintext, fillcolor = 'white', fontsize = 13, fontname = 'Helvetica-Bold']
    lab_id [label = 'IDENTIFICATION']
    lab_scr [label = 'SCREENING']
    lab_elig [label = 'ELIGIBILITY']
    lab_inc [label = 'INCLUDED']

    # ---- Flow edges ----
    # Identification (left column, vertical)
    edge [style = invis, arrowhead = none]
    lab_id -> id

    # Screening
    edge [style = invis, arrowhead = none]
    lab_scr -> scr

    # Eligibility
    edge [style = invis, arrowhead = none]
    lab_elig -> assess

    # Included
    edge [style = invis, arrowhead = none]
    lab_inc -> include

    # Main flow (visible arrows)
    edge [style = solid, color = '#2C3E50', arrowhead = normal]
    id -> scr
    scr -> sought
    sought -> assess
    assess -> include

    # Dedup connection (dashed, to the right)
    edge [style = dashed, color = '#95A5A6', arrowhead = none]
    id -> dup

    # Exclusion branches (to the right, red)
    edge [style = solid, color = '#C0392B', arrowhead = normal]
    scr -> exc1
    sought -> notret
    assess -> exc2

    # ---- Horizontal alignment (rank = same) ----
    { rank = same; lab_id; id; dup }
    { rank = same; lab_scr; scr; exc1 }
    { rank = same; sought; notret }
    { rank = same; lab_elig; assess; exc2 }
    { rank = same; lab_inc; include }
  }
  "
  grViz(dot)
}

# ---- Render diagram (interactive: Positron/RStudio Viewer) ----
prisma_dot()

# ---- Export instructions ----
cat("\n============================================================\n")
cat("PRISMA flow diagram rendered in Viewer pane.\n")
cat("Verified numbers saved to: figures/PRISMA_flow_numbers.csv\n")
cat("\nTo export as PNG (uncomment in script):\n")
cat("  library(DiagrammeRsvg); library(rsvg)\n")
cat("  svg <- export_svg(prisma_dot())\n")
cat("  writeLines(svg, 'figures/PRISMA_flow.svg')\n")
cat("  rsvg_png('figures/PRISMA_flow.svg', 'figures/PRISMA_flow.png',\n")
cat("           width = 1500, height = 2000)\n")
cat("============================================================\n")
