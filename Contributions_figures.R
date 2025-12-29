# ---- packages ----
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ============ SETTINGS ============
CSV_PATH <- "C:/R_scripts/PhD/country_PII_conts.csv"  # path to your CSV file
TOP_N <- 10                                       # number of countries to plot

# ---- LABELS (customize freely) ----
label_existing <- "Existing pressures contribution"
label_new      <- "New pressures contribution"
label_pii      <- "Potential Impact Intensity mean (PII) in Countries"
label_piishort <- "PII"

plot_title    <- paste0("Top ", TOP_N, " Countries by ", label_pii)
plot_subtitle <- NULL
plot_x_label  <- label_pii

# ---- PALETTE ----
pal <- setNames(c("#1f77b4", "#ff7f0e"),
                c(label_existing, label_new))

# ---- READ DATA ----
# CSV must have columns: Country, Existing, New, PII
df <- read_csv(CSV_PATH, show_col_types = FALSE)

# OPTIONAL: sanity checks (won't stop the script)
if (!all(c("Country","Existing","New","PII") %in% names(df))) {
  stop("CSV must contain columns: Country, Existing, New, PII")
}

# ---- SELECT TOP N ----
df_top <- df %>%
  arrange(desc(PII)) %>%
  slice_head(n = TOP_N) %>%
  mutate(Country = forcats::fct_reorder(Country, PII, .desc = TRUE))

# ---- PREPARE LONG DATA ----
df_long <- df_top %>%
  pivot_longer(c(Existing, New), names_to = "Type", values_to = "Share") %>%
  mutate(
    Type    = recode(Type, "Existing" = label_existing, "New" = label_new),
    Value   = Share * PII,
    Percent = Share * 100
  )

# ---- BUILD PLOT ----
fig1 <- ggplot(df_long, aes(x = Value, y = Country, fill = Type)) +
  geom_col(width = 0.7, colour = "grey15", linewidth = 0.2) +
  # % labels inside each stacked segment
  geom_text(
    aes(label = paste0(round(Percent, 0), "%")),
    position = position_stack(vjust = 0.5),
    color = "white", size = 3
  ) +
  # descriptive PII label at end of bar
  geom_text(
    data = df_top,
    inherit.aes = FALSE,
    aes(x = PII, y = Country,
        label = paste0(label_piishort, " = ", scales::number(PII, accuracy = 0.01))),
    hjust = -0.05, size = 3.3
  ) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = plot_x_label, y = NULL, fill = NULL,
    title = plot_title, subtitle = plot_subtitle
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

# ---- PRINT & SAVE ----
print(fig1)

png_height <- 0.4 * TOP_N + 2
output_dir <- "C:/R_scripts/PhD"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# unique name from CSV + TOP_N
csv_name <- tools::file_path_sans_ext(basename(CSV_PATH))
output_file <- file.path(output_dir, paste0("fig_", TOP_N, "_", csv_name, ".png"))

ggsave(output_file, fig1, width = 10, height = png_height, dpi = 300)
cat("??? Saved plot to:", output_file, "\n")
