# Requires input/ and output/ directories.
library(ggplot2)
library(ggalluvial)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - archive/hunting and trust/hunting-trust/")
survey = read.csv("intput/Binary_with_Qual_Trust_Clean_jrw22May2026.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
questions = c(BM = "TRUST2_text", HO = "TRUST4_text", PP = "TRUST6_text")
stopifnot(all(questions %in% names(survey)))

# T = agree/strongly agree; N = neutral; D = disagree/strongly disagree.
# Missing and "I have never considered this" stay unclassified.
classify = function(x) {
  result = rep(NA_character_, length(x))
  result[x %in% c("Agree", "Strongly agree")] = "T"
  result[x == "Neutral"] = "N"
  result[x %in% c("Disagree", "Strongly disagree")] = "D"
  result
}
classified = as.data.frame(lapply(survey[questions], classify))
names(classified) = names(questions)
answered_all = rowSums(!is.na(survey[questions]) & survey[questions] != "") == 3L
keep = complete.cases(classified)
cat("Total survey records:", nrow(survey), "\n")
cat("Answered all three (any response option):", sum(answered_all), "\n")
cat("Retained with T/N/D on all three:", sum(keep), "\n")
cat("Excluded from those who answered all three because at least one answer was 'never considered':", sum(answered_all & !keep), "\n")
profiles = classified[keep, , drop = FALSE]
profiles$profile = paste0(profiles$BM, profiles$HO, profiles$PP)
n_profiles = nrow(profiles)

# One row per possible response combination, including any combination with zero respondents.
axis_levels = c("T", "N", "D")
profile_levels = apply(expand.grid(axis_levels, axis_levels, axis_levels), 1, paste0, collapse = "")
profile_counts = as.data.frame(table(factor(profiles$profile, levels = profile_levels)))
names(profile_counts) = c("profile", "n")
profile_counts$percent = round(100 * profile_counts$n / n_profiles, 1)
profile_counts = profile_counts[order(-profile_counts$n), ]
rownames(profile_counts) = NULL
print(profile_counts, row.names = FALSE)
write.csv(profile_counts, "output/Figure1_TND_profile_counts.csv", row.names = FALSE)

axis_totals = do.call(rbind, lapply(names(questions), function(axis) {
  counts = table(factor(profiles[[axis]], levels = axis_levels))
  data.frame(dimension = axis, response = names(counts), n = as.integer(counts),
             percent = round(100 * as.integer(counts) / n_profiles, 1))
}))
rownames(axis_totals) = NULL
print(axis_totals, row.names = FALSE)
write.csv(axis_totals, "output/Figure1_TND_axis_totals.csv", row.names = FALSE)

# Each profile has separate resident and nonresident flows
profiles$residency = ifelse(
  survey$Resident_binary[keep] == 1,
  "Resident", "Non-resident"
)

flow_data = as.data.frame(table(
  profile = factor(profiles$profile, levels = profile_levels),
  residency = factor(profiles$residency, levels = c("Resident", "Non-resident"))
))
names(flow_data)[3] = "n"
flow_data = flow_data[flow_data$n > 0, ]
flow_data$BM = factor(substr(as.character(flow_data$profile), 1, 1), levels = axis_levels, labels = c("Trust", "Neutral", "Distrust"))
flow_data$HO = factor(substr(as.character(flow_data$profile), 2, 2), levels = axis_levels, labels = c("Trust", "Neutral", "Distrust"))
flow_data$PP = factor(substr(as.character(flow_data$profile), 3, 3),levels = axis_levels, labels = c("Trust", "Neutral", "Distrust"))

flow_data$flow_color = factor(
  paste(flow_data$BM, flow_data$residency, sep = " - "),
  levels = c(
    "Trust - Resident", "Trust - Non-resident",
    "Neutral - Resident", "Neutral - Non-resident",
    "Distrust - Resident", "Distrust - Non-resident"
))

figure1 = ggplot(
  flow_data,
  aes(axis1 = BM, axis2 = HO, axis3 = PP, y = n)
) +
  geom_alluvium(
    aes(fill = flow_color),
    alpha = 0.85,
    width = 0.15,
    color = "white",
    linewidth = 0.12
  ) +
  geom_stratum(
    width = 0.15,
    fill = "grey94",
    color = "grey30"
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Trust - Resident" = "#A9D9AD",
      "Trust - Non-resident" = "#237A3B",
      "Neutral - Resident" = "#F2D66D",
      "Neutral - Non-resident" = "#B58A00",
      "Distrust - Resident" = "#CBA9DC",
      "Distrust - Non-resident" = "#6F3B87"
    ),
    name = "Biological Management / residency"
  ) +
  scale_x_discrete(
    limits = c(
      "Biological Management",
      "Hunting Opportunity",
      "Public Participation"
    ),
    expand = c(0.08, 0.08)
  ) +
  labs(
    x = NULL,
    y = "Number of respondents",
    subtitle = paste0(
      "Trust, Neutral, or Distrust on all three questions (n = ",
      format(n_profiles, big.mark = ","),
      ")"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 10),
    legend.position = "right"
  )

print(figure1)
ggsave(
  "output/Figure1_TND_flows_by_residency.pdf",
  figure1,
  width = 11,
  height = 6
)

# Companion table 
profiles$residency = ifelse(survey$Resident_binary[keep] == 1,"Resident", "Non-resident")
residency_levels = c("Resident", "Non-resident")
by_residency = table(factor(profiles$profile, levels = profile_levels), factor(profiles$residency, levels = residency_levels))
profile_residency = data.frame(
  profile = profile_levels,
  resident = as.integer(by_residency[, "Resident"]),
  nonresident = as.integer(by_residency[, "Non-resident"])
)
profile_residency$total = profile_residency$resident + profile_residency$nonresident
profile_residency$overall_percent = round(100 * profile_residency$total / n_profiles, 1)
profile_residency$resident_percent = round(100 * profile_residency$resident / sum(profile_residency$resident), 1)
profile_residency$nonresident_percent = round(100 * profile_residency$nonresident / sum(profile_residency$nonresident), 1)
profile_residency = profile_residency[order(-profile_residency$total), ]
rownames(profile_residency) = NULL
cat("Profile counts by residency (group percentages use separate denominators):\n")
print(profile_residency, row.names = FALSE)
write.csv(profile_residency, "output/Figure1_TND_profile_by_residency.csv",row.names = FALSE)
cat("Resident n =", sum(profile_residency$resident),
    "; nonresident n =", sum(profile_residency$nonresident), "\n")

axis_totals = do.call(rbind, lapply(names(questions), function(axis) {
  counts = table(factor(profiles[[axis]], levels = axis_levels))
  data.frame(dimension = axis, response = names(counts), n = as.integer(counts),
             percent = round(100 * as.integer(counts) / n_profiles, 1))
}))
rownames(axis_totals) = NULL
print(axis_totals, row.names = FALSE)
write.csv(axis_totals, "output/Figure1_TND_axis_totals.csv", row.names = FALSE)

