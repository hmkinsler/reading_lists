# compare_reading_lists.R
#
# Cross-list analysis: which cited works appear across multiple reading list
# areas, and how do the lists relate to each other intellectually?
#
# Produces three plots:
#   1. Heatmap — source papers (rows) × shared cited works (columns)
#   2. Overlap bar chart — works cited in 2+ lists
#   3. Combined network — all lists in one graph, coloured by list membership
#
# Usage: Set BASE_DIR below, then source() or Run All.
# Output is saved to <BASE_DIR>/visualizations/.

# ── Configuration ─────────────────────────────────────────────────────────────

BASE_DIR <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) normalizePath(".")  # falls back to current working directory
)

# Automatically discover all subdirectories that contain a citation_network.csv
list_dirs <- list.dirs(BASE_DIR, full.names = TRUE, recursive = FALSE) |>
  keep(~ file.exists(file.path(.x, "citation_network.csv")))

if (length(list_dirs) == 0) stop("No citation_network.csv files found under ", BASE_DIR)

# ── Packages ──────────────────────────────────────────────────────────────────

required <- c("tidyverse", "igraph", "ggraph", "tidygraph", "scales")
missing  <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(scales)

# ── Load all lists ────────────────────────────────────────────────────────────

all_edges <- map_dfr(list_dirs, function(d) {
  read_csv(file.path(d, "citation_network.csv"), show_col_types = FALSE)
}) |>
  filter(!is.na(target_title), target_title != "") |>
  mutate(
    source_id = if_else(!is.na(source_doi) & source_doi != "", source_doi, source_title),
    target_id = if_else(!is.na(target_doi) & target_doi != "", target_doi, target_title),
    list_label = str_to_title(str_replace_all(reading_list, "_", " "))
  )

reading_lists <- sort(unique(all_edges$list_label))
list_colors   <- setNames(
  hue_pal()(length(reading_lists)),
  reading_lists
)

# ── 1. Shared-citation heatmap ────────────────────────────────────────────────
# Rows = source papers, columns = cited works appearing in 2+ lists

shared_targets <- all_edges |>
  distinct(target_id, reading_list) |>
  count(target_id, name = "n_lists") |>
  filter(n_lists >= 2)

heatmap_data <- all_edges |>
  filter(target_id %in% shared_targets$target_id) |>
  distinct(source_title, target_title, list_label) |>
  mutate(
    source_short = str_trunc(source_title, 45),
    target_short = str_trunc(target_title, 55)
  )

if (nrow(heatmap_data) > 0) {
  # Keep only the top 30 shared targets by total appearances to keep readable
  top_shared_targets <- all_edges |>
    filter(target_id %in% shared_targets$target_id) |>
    count(target_title, name = "total") |>
    slice_max(total, n = 30) |>
    pull(target_title)

  heatmap_plot_data <- heatmap_data |>
    filter(target_title %in% top_shared_targets) |>
    mutate(cited = TRUE)

  p_heat <- ggplot(
    heatmap_plot_data,
    aes(x = target_short, y = source_short, fill = list_label)
  ) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_manual(values = list_colors, name = "Reading list") +
    labs(
      title    = "Shared Citations Across Reading Lists",
      subtitle = "Each cell = a source paper cites a work that appears in 2+ lists",
      x        = "Cited work",
      y        = "Source paper"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y   = element_text(size = 7),
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "grey50"),
      legend.position = "top"
    )

  print(p_heat)
} else {
  message("No works shared across lists — skipping heatmap.")
}

# ── 2. Cross-list overlap bar chart ───────────────────────────────────────────

overlap <- all_edges |>
  distinct(target_id, target_title, reading_list) |>
  group_by(target_id, target_title) |>
  summarise(
    n_lists      = n_distinct(reading_list),
    lists_joined = paste(sort(unique(
      str_to_title(str_replace_all(reading_list, "_", " "))
    )), collapse = " + "),
    .groups = "drop"
  ) |>
  filter(n_lists >= 2) |>
  arrange(desc(n_lists), target_title)

if (nrow(overlap) > 0) {
  p_overlap <- overlap |>
    slice_head(n = 30) |>
    mutate(short_title = str_trunc(target_title, 65)) |>
    ggplot(aes(x = n_lists, y = reorder(short_title, n_lists), fill = lists_joined)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = n_lists), hjust = -0.3, size = 3) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.15)),
      breaks = 1:length(reading_lists)
    ) +
    scale_fill_brewer(palette = "Set2", name = "Lists") +
    labs(
      title    = "Works Cited Across Multiple Reading Lists",
      subtitle = "These are the cross-cutting foundational texts for your quals",
      x        = "Number of lists that cite this work",
      y        = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey50", size = 9),
      axis.text.y   = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )

  print(p_overlap)

  cat("\n=== Works appearing in multiple reading lists ===\n")
  overlap |>
    select(Title = target_title, Lists = lists_joined, `# Lists` = n_lists) |>
    print(n = Inf, width = 120)
} else {
  message("No overlapping citations found between lists.")
}

# ── 3. Combined network coloured by reading list ──────────────────────────────

# Identify source nodes per list
source_per_list <- all_edges |>
  distinct(node_id = source_id, label = source_title, list_label) |>
  mutate(node_type = "source")

# For citation nodes, assign the list of the majority of sources that cite them
citation_list_assign <- all_edges |>
  count(target_id, target_title, list_label) |>
  group_by(target_id, target_title) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(node_id = target_id, label = target_title,
            list_label, node_type = "citation")

nodes_combined <- bind_rows(source_per_list, citation_list_assign) |>
  distinct(node_id, .keep_all = TRUE) |>
  mutate(
    node_type  = if_else(node_id %in% source_per_list$node_id, "source", "citation"),
    short_label = str_trunc(label, 40)
  )

# Citation counts for node sizing
cite_counts_all <- all_edges |>
  count(target_id, name = "cite_count")

nodes_combined <- nodes_combined |>
  left_join(cite_counts_all, by = c("node_id" = "target_id")) |>
  mutate(
    cite_count = replace_na(cite_count, 0),
    node_size  = if_else(node_type == "source", 5, log1p(cite_count) + 1)
  )

# Only label source nodes and highly cross-cited works in combined view
cross_cited_ids <- overlap |>
  filter(n_lists >= 2) |>
  pull(target_id)

nodes_combined <- nodes_combined |>
  mutate(show_label = node_type == "source" | node_id %in% cross_cited_ids)

edges_combined <- all_edges |> select(from = source_id, to = target_id)

g_combined <- graph_from_data_frame(edges_combined, vertices = nodes_combined, directed = TRUE) |>
  as_tbl_graph()

set.seed(42)

p_combined <- ggraph(g_combined, layout = "fr") +
  geom_edge_link(
    alpha  = 0.08,
    colour = "grey70",
    arrow  = arrow(length = unit(1.5, "mm"), type = "closed"),
    end_cap = circle(2, "mm")
  ) +
  geom_node_point(
    aes(color = list_label, size = node_size, shape = node_type),
    alpha = 0.85
  ) +
  geom_node_label(
    aes(
      label = if_else(show_label, str_wrap(short_label, 30), NA_character_),
      fill  = list_label
    ),
    repel        = TRUE,
    size         = 2.2,
    label.size   = 0.1,
    label.r      = unit(0.12, "lines"),
    alpha        = 0.85,
    color        = "white",
    na.rm        = TRUE,
    max.overlaps = 25
  ) +
  scale_color_manual(values = list_colors, name = "Reading list") +
  scale_fill_manual(values  = list_colors, guide = "none") +
  scale_shape_manual(
    values = c(source = 15, citation = 19),
    labels = c(source = "Source paper", citation = "Cited work"),
    name   = NULL
  ) +
  scale_size_identity() +
  labs(
    title    = "Combined Citation Network — All Reading Lists",
    subtitle = "Labels on source papers and works cited across multiple lists"
  ) +
  theme_graph(base_family = "sans", base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    legend.position = "bottom"
  )

print(p_combined)

# ── Console summary ───────────────────────────────────────────────────────────

cat(sprintf("\n=== Combined summary across %d reading lists ===\n", length(reading_lists)))
all_edges |>
  group_by(list_label) |>
  summarise(
    sources  = n_distinct(source_id),
    cited    = n_distinct(target_id),
    edges    = n(),
    .groups  = "drop"
  ) |>
  print()
cat(sprintf("\nWorks shared across 2+ lists : %d\n", nrow(overlap)))
