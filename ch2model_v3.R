# ==============================================================================
# v3.31  - Active restoration adds larvae directly as settled at target reef
#         (bypasses dispersal; 100% stays at reef); year 1 only (LINE 99)
#       - 140 M restoration larvae from 1 billion 
#       - calibrating juvenile densities (6.9 ind m2 from GBR -> 6.12 ± 0.56 ind m2) 
#           -> maturity 9%, larvae per hcc 100M, 2 cm2 larvae area
#       - changed passive to management on plot
#       - add bioregion to output table, with left join
# ==============================================================================

if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(openxlsx)) install.packages("openxlsx"); library(openxlsx)
if (!require(igraph)) install.packages("igraph"); library(igraph)
if (!require(parallel)) install.packages("parallel"); library(parallel)
if (!require(foreach)) install.packages("foreach"); library(foreach)
if (!require(doParallel)) install.packages("doParallel"); library(doParallel)


# ==============================================================================
# LOAD DATA
# ==============================================================================
version <- 3.32
data_path <- "data/"
nodes <- read.csv(paste(data_path,"centroid.csv",sep="")) #2404 ReefIDs with HCC, reef area, type, gravity penalty, mortality rate
edges <- read.csv(paste(data_path,"sourcesink.csv",sep="")) #full ReefID source-sink pairs with settlement probabilities
reeftype <-read.csv(paste(data_path,"type.csv",sep="")) #HACC & intrinsic growth per year by reef type
mmgrow <-read.csv(paste(data_path,"composition.csv",sep="")) #HACC & intrinsic growth per year by reef type in mm/year
#preselect <- read.csv(paste(output_path,"preselect-ids.csv",sep="")) #top reefids based on net hcc

# ==============================================================================
# DATA CLEANING & INTERNATIONAL REEF HANDLING
# ==============================================================================
setnames(nodes,
         old=c("ReefID","ave_hcc", "ReefArea.km.sq.", "type_assumption"),
         new=c("reefid","hcc", "area_kmsq", "type"), skip_absent = TRUE)

setnames(edges, old=c("Probability.of.Settlement"), new=c("p_settle"), skip_absent = TRUE)

nodes$reefid <- as.character(nodes$reefid)
edges <- data.frame(
  source   = as.character(edges$SourceReef),
  sink     = as.character(edges$SinkReef),
  p_settle = as.numeric(edges$p_settle)
)

# Combine and Join Parameters
mmgrow$mmyr <- as.numeric (mmgrow$mmyr) #growth in mm as numeric
mmgrow_agg = mmgrow %>% group_by(type) %>% summarise(mmyr = sum(mmyr * percont), bleachmort = sum(bleachmort * percont), typhmort = sum(typhmort * percont), .groups = 'drop')
reef_params <- mmgrow_agg %>% left_join(reeftype, by = "type")

all_edge_reefs <- unique(c(edges$source, edges$sink)) #lists all nodes affecting PH nodes

international_nodes <- data.frame(reefid = setdiff(all_edge_reefs, nodes$reefid), area_kmsq = 1, type = "I", hcc = 1, penalty = 0, typhprob = 0, bleachprob = 0, is_philippines = FALSE)

nodes$is_philippines <- TRUE

nodes_final <- bind_rows(nodes, international_nodes) %>% left_join(reef_params, by = "type") %>%
  mutate(mmyr = ifelse(is.na(mmyr), 18.3, mmyr), typhmort = ifelse(is.na(typhmort), 0.02, typhmort), bleachmort = ifelse(is.na(bleachmort), 0.02, bleachmort),
         K = ifelse(is.na(hacc), 57.2, hacc), growthrate = ifelse(is.na(growthrate), 4.9, growthrate)/100) %>% distinct(reefid, .keep_all = TRUE)

# List reefs for group intervention
preselected_ids <- c("1228", "2768", "2122", "3428", 
                     "1558", "1967", "908", "2697", 
                     "3476", "3199") #default top 10 active resto candidates

#OR from a file:
#preselected_ids <- preselect |>
#  filter(act. > 0) |> #here choose act., pass., or comb.
#  pull(reefid)

target_indices <- which(nodes_final$reefid %in% preselected_ids)

# ==============================================================================
# BUILD NETWORK & EXTRACT MATRICES
# ==============================================================================
coral_graph <- graph_from_data_frame(d = edges, vertices = nodes_final, directed = TRUE)
adj_mat <- as_adjacency_matrix(coral_graph, attr = "p_settle", sparse = TRUE)
P_T <- Matrix::t(adj_mat)

REEF      <- V(coral_graph)$name
HCC_INIT  <- V(coral_graph)$hcc
AREA      <- V(coral_graph)$area_kmsq
PENALTY   <- V(coral_graph)$penalty
K_VALS    <- V(coral_graph)$K
R_GROW    <- V(coral_graph)$growthrate
IS_PH     <- V(coral_graph)$is_philippines
# Expected Annual Mortality (Deterministic approach for speed)
EXP_MORT  <- pmin(0.95, (V(coral_graph)$typhprob * V(coral_graph)$typhmort) +
                    (V(coral_graph)$bleachprob * V(coral_graph)$bleachmort))

# Constants
LARVAE_PER_HCC     <- 100000 #note HCC 0-100, so actually 100M
MATURITY_RATE      <- 0.09
COLONY_TO_KMSQ     <- 0.0000000002
STEPS              <- 25
RESTORATION_LARVAE <- 1.4e8 #e7

# ==============================================================================
# SIMULATION FUNCTION
# ==============================================================================
run_core_logic_ts <- function(hcc_start, total_area, steps, p_vec, r_indices = NULL, r_larvae = 0) {
  ts_matrix      <- matrix(0, nrow = length(hcc_start), ncol = steps)
  recruit_matrix <- matrix(0, nrow = length(hcc_start), ncol = steps)
  settled_matrix <- matrix(0, nrow = length(hcc_start), ncol = steps)
  current_hcc    <- hcc_start

  for (t in 1:steps) {
    larvae  <- current_hcc * total_area * LARVAE_PER_HCC
    settled <- as.vector(P_T %*% larvae)
    if (t == 1 && !is.null(r_indices)) {
      # Added larvae injected directly as settled (no dispersal; 100% stays at target)
      settled[r_indices] <- settled[r_indices] + r_larvae
    }
    recruit <- ((settled * MATURITY_RATE * COLONY_TO_KMSQ) / AREA * 100) * (1 - current_hcc / K_VALS)
    growth  <- (pmax(0, R_GROW * (1 - p_vec))) * current_hcc * (1 - current_hcc / K_VALS)
    current_hcc <- (current_hcc + growth + recruit) * (1 - EXP_MORT)
    current_hcc <- pmin(K_VALS, pmax(0, current_hcc))
    ts_matrix[, t]      <- current_hcc
    recruit_matrix[, t] <- recruit
    settled_matrix[, t] <- settled
  }
  list(hcc = ts_matrix, recruit = recruit_matrix, settled = settled_matrix)
}

# ==============================================================================
# RUN INDIVIDUALIZED SCENARIOS (2-4: Hotspot Discovery)
# ==============================================================================
cat("Calculating S1 Baseline...\n")
s1_result <- run_core_logic_ts(HCC_INIT, AREA, STEPS, PENALTY)
s1_ts     <- s1_result$hcc

ph_indices <- which(IS_PH)
n_cores <- max(1, detectCores() - 1)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterExport(cl, c("run_core_logic_ts", "P_T", "HCC_INIT", "PENALTY", "AREA", "K_VALS",
                    "R_GROW", "EXP_MORT", "LARVAE_PER_HCC", "MATURITY_RATE",
                    "COLONY_TO_KMSQ", "STEPS", "RESTORATION_LARVAE"))
clusterEvalQ(cl, library(Matrix))

cat(paste("Running Localized Hotspot Analysis (S2-S4) on", n_cores, "cores...\n"))

individual_results <- foreach(idx = ph_indices, .options.snow = list(preschedule = TRUE)) %dopar% {
  local_penalty <- PENALTY
  local_penalty[idx] <- 0

  s2 <- run_core_logic_ts(HCC_INIT, AREA, STEPS, PENALTY,       r_indices = idx, r_larvae = RESTORATION_LARVAE)
  s3 <- run_core_logic_ts(HCC_INIT, AREA, STEPS, local_penalty)
  s4 <- run_core_logic_ts(HCC_INIT, AREA, STEPS, local_penalty, r_indices = idx, r_larvae = RESTORATION_LARVAE)

  list(
    final      = c(s2$hcc[idx, STEPS], s3$hcc[idx, STEPS], s4$hcc[idx, STEPS]),
    ts         = rbind(s2$hcc[idx, ], s3$hcc[idx, ], s4$hcc[idx, ]),
    s4_recruit = s4$recruit[idx, ],
    s4_settled = s4$settled[idx, ]
  )
}
stopCluster(cl)


# ==============================================================================
# RUN PORTFOLIO SCENARIOS (5-7: Group Intervention)
# ==============================================================================
cat("Calculating Portfolio Scenarios (5-7)...\n")
portfolio_penalty <- PENALTY
portfolio_penalty[target_indices] <- 0

s5_result <- run_core_logic_ts(HCC_INIT, AREA, STEPS, PENALTY,          r_indices = target_indices, r_larvae = RESTORATION_LARVAE)
s6_result <- run_core_logic_ts(HCC_INIT, AREA, STEPS, portfolio_penalty)
s7_result <- run_core_logic_ts(HCC_INIT, AREA, STEPS, portfolio_penalty, r_indices = target_indices, r_larvae = RESTORATION_LARVAE)
s5_ts <- s5_result$hcc
s6_ts <- s6_result$hcc
s7_ts <- s7_result$hcc


# ==============================================================================
# BUILD OUTPUT DATAFRAMES
# ==============================================================================
ph_reefs <- REEF[IS_PH]
n_ph     <- sum(IS_PH)

# Individual gain data at end of simulation (S2-S4)
indiv_reefgain_df <- data.frame(
  reefid = REEF[ph_indices],
  S1_baseline = s1_ts[ph_indices, STEPS],
  do.call(rbind, lapply(individual_results, `[[`, "final")) %>% `colnames<-`(c("S2_active", "S3_passive", "S4_combined"))
) %>% mutate(
  active_gain = S2_active - S1_baseline,
  passive_gain = S3_passive - S1_baseline,
  combined_gain = S4_combined - S1_baseline
)

# Individual HCC time series (S2-S4): HCC of each reef when it was the sole target
indiv_ts_df <- bind_rows(lapply(seq_along(ph_indices), function(i) {
  ts <- individual_results[[i]]$ts
  data.frame(
    reefid           = REEF[ph_indices[i]],
    year             = 1:STEPS,
    S2_active        = ts[1, ],
    S3_passive       = ts[2, ],
    S4_combined      = ts[3, ],
    S4_recruit_hcc   = individual_results[[i]]$s4_recruit,
    S4_settled_larvae = individual_results[[i]]$s4_settled
  )
}))

# Archives all yearly HC for all PH reefs. Note that S2-S4 indicates HC if individual intervention while S5-S7 are for group interventions based on preselected_ids
reef_yearly_ts <- data.frame(
  reefid             = rep(ph_reefs, times = STEPS),
  year               = rep(1:STEPS, each = n_ph),
  # HCC columns
  S1_baseline        = as.vector(s1_ts[IS_PH, ]),
  S2_active          = NA_real_,
  S3_passive         = NA_real_,
  S4_combined        = NA_real_,
  S5_active          = as.vector(s5_ts[IS_PH, ]),
  S6_passive         = as.vector(s6_ts[IS_PH, ]),
  S7_combined        = as.vector(s7_ts[IS_PH, ]),
  # Recruitment HCC contribution per reef per year (% HCC units added by settlers)
  S1_recruit_hcc     = as.vector(s1_result$recruit[IS_PH, ]),
  S4_recruit_hcc     = NA_real_,   # filled per-reef from parallel loop
  S5_recruit_hcc     = as.vector(s5_result$recruit[IS_PH, ]),
  S6_recruit_hcc     = as.vector(s6_result$recruit[IS_PH, ]),
  S7_recruit_hcc     = as.vector(s7_result$recruit[IS_PH, ]),
  # Settled larvae per reef per year (raw count before maturity rate applied)
  S1_settled_larvae  = as.vector(s1_result$settled[IS_PH, ]),
  S4_settled_larvae  = NA_real_,   # filled per-reef from parallel loop
  S5_settled_larvae  = as.vector(s5_result$settled[IS_PH, ]),
  S6_settled_larvae  = as.vector(s6_result$settled[IS_PH, ]),
  S7_settled_larvae  = as.vector(s7_result$settled[IS_PH, ]),
  is_targeted        = rep(ph_reefs %in% preselected_ids, times = STEPS)
) %>% rows_update(indiv_ts_df, by = c("reefid", "year"), unmatched = "ignore")


# End of simulation gain for S5-S7
group_reefgain_df <- data.frame(
  reefid  = ph_reefs,
  S5_gain = s5_ts[IS_PH, STEPS] - s1_ts[IS_PH, STEPS],
  S6_gain = s6_ts[IS_PH, STEPS] - s1_ts[IS_PH, STEPS],
  S7_gain = s7_ts[IS_PH, STEPS] - s1_ts[IS_PH, STEPS]
)

# PH-wide annual totals: sum recruit and settled across all PH reefs per year
# S2/S3/S4 individual runs each target one reef at a time, so a single
# PH-wide total per year is not meaningful for those scenarios — only
# S1 (baseline) and S5-S7 (fixed portfolio) are included here.
annual_ph_recruit_df <- data.frame(
  year                  = 1:STEPS,
  S1_recruit_hcc        = colSums(s1_result$recruit[IS_PH, ]),
  S5_recruit_hcc        = colSums(s5_result$recruit[IS_PH, ]),
  S6_recruit_hcc        = colSums(s6_result$recruit[IS_PH, ]),
  S7_recruit_hcc        = colSums(s7_result$recruit[IS_PH, ]),
  S1_settled_larvae     = colSums(s1_result$settled[IS_PH, ]),
  S5_settled_larvae     = colSums(s5_result$settled[IS_PH, ]),
  S6_settled_larvae     = colSums(s6_result$settled[IS_PH, ]),
  S7_settled_larvae     = colSums(s7_result$settled[IS_PH, ])
)

# indiv_reefgain_df transform for visualization
indiv_reefgain_long <- indiv_reefgain_df %>%
  dplyr::select(reefid, active_gain, passive_gain, combined_gain) %>%
  pivot_longer(-reefid, names_to = "intervention", values_to = "gain") %>%
  dplyr::mutate(intervention = dplyr::recode(intervention,
    active_gain   = "S2 Active",
    passive_gain  = "S3 Management",
    combined_gain = "S4 Combined"
  ))

# group_reefgain_df transform for visualization
group_reefgain_long <- group_reefgain_df %>%
  pivot_longer(-reefid, names_to = "intervention", values_to = "gain") %>%
  mutate(intervention = recode(intervention,
    S5_gain = "S5 Active",
    S6_gain = "S6 Passive",
    S7_gain = "S7 Combined"
  ))

# group_reefgain_df transform for visualization (targeted reefs only)
group_reefgain_targeted <- group_reefgain_df %>%
  filter(reefid %in% preselected_ids) %>%
  pivot_longer(-reefid, names_to = "intervention", values_to = "gain") %>%
  mutate(intervention = recode(intervention,
    S5_gain = "S5 Active",
    S6_gain = "S6 Passive",
    S7_gain = "S7 Combined"
  ))

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# Individual reef HCC gain vs baseline — all PH reefs (S2-S4)

#indivgain20 <- indiv_reefgain_long |>
#  filter(gain < 20) #remove 2 outliers 1108 & 3428
#summary(indivgain20)
indiv_reefgain_ph_boxplot <- ggplot(indiv_reefgain_long, aes(x = intervention, y = gain, fill = intervention)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = paste0("Individual Reef Potential Model ver. ", 
                      version, "-", RESTORATION_LARVAE), y = paste0("% Gain in HCC (Yr ", STEPS, ")"), x = "Intervention") +
  theme_minimal()
print(indiv_reefgain_ph_boxplot)

# Network-wide HCC gain vs baseline — all PH reefs (S5-S7)
group_reefgain_ph_boxplot <- ggplot(group_reefgain_long, aes(x = intervention, y = gain, fill = intervention)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "Portfolio Network-Wide Gain vs Baseline (S5-S7)",
       y = paste0("HCC Gain (Yr ", STEPS, ")")) +
  theme_minimal()

# HCC gain vs baseline — targeted portfolio reefs only (S5-S7)
group_reefgain_target_boxplot <- ggplot(group_reefgain_targeted, aes(x = intervention, y = gain, fill = intervention)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = paste0("Portfolio Gain — Targeted Reefs (S5-S7 vs S1, n=", length(preselected_ids), ")"),
       y = paste0("HCC Gain (Yr ", STEPS, ")")) +
  theme_minimal()

print(group_reefgain_ph_boxplot)
print(group_reefgain_target_boxplot)

#indiv_reefgain_df$bioregion = nodes$bioregion
indiv_reefgain_df <- indiv_reefgain_df %>%
  left_join(nodes %>% dplyr::select(reefid, bioregion), by = "reefid")

# ==============================================================================
# EXPORT
# ==============================================================================
output_file <- paste0("output/phresto-", version, "-", format(Sys.Date(), "%Y%m%d"), ".xlsx")
write.xlsx(
  list(
    "Yearly_HCC"              = reef_yearly_ts,
    "Annual_PH_Recruit"       = annual_ph_recruit_df,
    "Individual Intervention" = indiv_reefgain_df,
    "Group Intervention"      = group_reefgain_df
  ),
  file = output_file
)
cat(paste("Exported:", output_file, "\n"))
cat("Model DONE!\n")
