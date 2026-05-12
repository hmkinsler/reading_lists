# visualize_citation_network.R
#
# Produces two plots for a single reading list's citation network:
#   1. Force-directed network graph (source papers + their citations)
#   2. Bar chart of top-cited works across the reading list
#
# Usage: Set BASE_DIR and READING_LIST below, then source() or Run All.
# Outputs are saved into <BASE_DIR>/<READING_LIST>/.

# ── Configuration ────────────────────────────────────────────────────────────

BASE_DIR <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) normalizePath(".")  # falls back to current working directory
)
READING_LIST <- "critical_making"   # change to "new_materialism", etc.
TOP_N_CITED  <- 20                  # how many top-cited works to label in the network
LABEL_WIDTH  <- 35                  # characters before wrapping node labels

# ── Packages ─────────────────────────────────────────────────────────────────

required <- c("tidyverse", "igraph", "ggraph", "tidygraph", "ggrepel")
missing  <- required[!sapply(required, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)

# ── Load & clean data ─────────────────────────────────────────────────────────

csv_path <- file.path(BASE_DIR, READING_LIST, "citation_network.csv")
edges_raw <- read_csv(csv_path, show_col_types = FALSE)

edges <- edges_raw |>
  filter(!is.na(target_title), target_title != "") |>
  mutate(
    # Prefer DOI as stable node ID; fall back to title
    source_id = if_else(!is.na(source_doi) & source_doi != "", source_doi, source_title),
    target_id = if_else(!is.na(target_doi) & target_doi != "", target_doi, target_title)
  )

# ── Build node & edge tables ──────────────────────────────────────────────────

source_nodes <- edges |>
  distinct(node_id = source_id, label = source_title) |>
  mutate(node_type = "source")

cited_nodes <- edges |>
  distinct(node_id = target_id, label = target_title) |>
  mutate(node_type = "citation")

nodes <- bind_rows(source_nodes, cited_nodes) |>
  # Reading-list papers that also appear as citations stay marked "source"
  distinct(node_id, .keep_all = TRUE) |>
  mutate(
    node_type   = if_else(node_id %in% source_nodes$node_id, "source", "citation"),
    short_label = str_trunc(label, LABEL_WIDTH)
  )

edge_list <- edges |> select(from = source_id, to = target_id)

# ── Compute citation counts for sizing/labelling ──────────────────────────────

cite_counts <- edges |>
  count(target_id, name = "cite_count")

nodes <- nodes |>
  left_join(cite_counts, by = c("node_id" = "target_id")) |>
  mutate(
    cite_count = replace_na(cite_count, 0),
    # Sources get fixed size; cited works scale with how often they appear
    node_size  = if_else(node_type == "source", 5, log1p(cite_count) + 1),
    # Label sources always; label the top-N most-cited works
    show_label = node_type == "source" |
      (node_type == "citation" & cite_count >= sort(
        cite_counts$cite_count, decreasing = TRUE
      )[min(TOP_N_CITED, nrow(cite_counts))])
  )

# ── Build igraph / tidygraph object ───────────────────────────────────────────

g <- graph_from_data_frame(edge_list, vertices = nodes, directed = TRUE) |>
  as_tbl_graph()

# ── Plot 1: Force-directed network ────────────────────────────────────────────

set.seed(42)

p_network <- ggraph(g, layout = "fr") +
  geom_edge_link(
    alpha = 0.12,
    colour = "grey60",
    arrow  = arrow(length = unit(1.8, "mm"), type = "closed"),
    end_cap = circle(2.5, "mm")
  ) +
  geom_node_point(
    aes(color = node_type, size = node_size, shape = node_type)
  ) +
  geom_node_label(
    aes(
      label = if_else(show_label, str_wrap(short_label, LABEL_WIDTH), NA_character_),
      fill  = node_type
    ),
    repel       = TRUE,
    size        = 2.4,
    label.size  = 0.15,
    label.r     = unit(0.15, "lines"),
    alpha       = 0.85,
    color       = "white",
    na.rm       = TRUE,
    max.overlaps = 20
  ) +
  scale_color_manual(
    values = c(source = "#C1121F", citation = "#1D6A96"),
    labels = c(source = "Reading list source", citation = "Cited work")
  ) +
  scale_fill_manual(
    values = c(source = "#C1121F", citation = "#1D6A96"),
    guide  = "none"
  ) +
  scale_shape_manual(
    values = c(source = 15, citation = 19),
    labels = c(source = "Reading list source", citation = "Cited work")
  ) +
  scale_size_identity() +
  labs(
    title    = paste("Citation Network —",
                     str_to_title(str_replace_all(READING_LIST, "_", " "))),
    subtitle = sprintf(
      "%d source papers  ·  %d unique cited works  ·  %d edges",
      nrow(source_nodes),
      sum(nodes$node_type == "citation"),
      nrow(edge_list)
    ),
    color = NULL, shape = NULL
  ) +
  theme_graph(base_family = "sans", base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    legend.position = "bottom"
  )

print(p_network)

# ── Plot 2: Top cited works (bar chart) ───────────────────────────────────────

top_cited <- edges |>
  count(target_id, target_title, name = "n_sources") |>
  arrange(desc(n_sources)) |>
  slice_head(n = 25) |>
  mutate(short_title = str_trunc(target_title, 65))

p_bar <- ggplot(top_cited, aes(x = n_sources, y = reorder(short_title, n_sources))) +
  geom_col(fill = "#1D6A96", width = 0.7) +
  geom_text(aes(label = n_sources), hjust = -0.25, size = 3.2, color = "grey30") +
  scale_x_continuous(
    expand  = expansion(mult = c(0, 0.12)),
    breaks  = scales::breaks_pretty()
  ) +
  labs(
    title    = paste("Most-Cited Works —",
                     str_to_title(str_replace_all(READING_LIST, "_", " "))),
    subtitle = "Count = number of reading list sources that cite each work",
    x        = "Times cited by sources",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey50", size = 9),
    axis.text.y   = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p_bar)

# ── Console summary ───────────────────────────────────────────────────────────

cat(sprintf("\n=== %s ===\n", str_to_upper(READING_LIST)))
cat(sprintf("Source papers  : %d\n", nrow(source_nodes)))
cat(sprintf("Cited works    : %d\n", sum(nodes$node_type == "citation")))
cat(sprintf("Citation edges : %d\n", nrow(edge_list)))
cat("\nTop 10 most-cited works:\n")
top_cited |>
  slice_head(n = 10) |>
  select(Cited = n_sources, Title = target_title) |>
  print(n = 10, width = 100)
