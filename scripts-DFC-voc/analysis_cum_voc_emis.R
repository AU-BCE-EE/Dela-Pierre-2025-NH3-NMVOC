# ============================================================
# Cumulative VOC emissions - effect of acidification treatment
# ============================================================
# This script:
#   1. Loads cumulative emission data and removes the "Machine plot" group
#   2. Fits a linear model (ANOVA-style) of total_cum ~ group
#   3. Visualizes cumulative emissions across acidification levels
#   4. Runs a Williams' test / Dunnett test for comparisons against
#      the "No acid" control (one-sided, "greater" alternative)
#   5. Checks whether VFA explains the group effect on total_cum (ANCOVA)
#


library(readr)
library(data.table)
library(ggplot2)     # needed for ggplot() calls below
library(PMCMRplus)   # needed for williamsTest()
library(multcomp)    # needed for glht() and mcp()

# ------------------------------------------------------------
# 1. Load and prepare data
# ------------------------------------------------------------
data <- read_csv("../output-DFC/cum.voc.emis.csv")
setDT(data)

# Remove "Machine plot" group - not part of the acidification comparison
data_anova <- data[group != "Machine plot"]

# Set factor levels, with "No acid" as the reference level for comparisons
data_anova[, group := relevel(factor(group), ref = "No acid")]

# ------------------------------------------------------------
# 2. Linear model: does acidification affect cumulative emissions?
# ------------------------------------------------------------
model_cum <- lm(total_cum ~ group, data = data_anova)
summary(model_cum)

# Re-order factor levels for plotting/readability (low -> high acid)
data_anova[, group := factor(group, levels = c("No acid", "Low acid", "Medium acid", "High acid"))]

# ------------------------------------------------------------
# 3. Visualization
# ------------------------------------------------------------

# Points + mean trend line (connecting group means)
ggplot(data_anova, aes(group, total_OAV, color = group)) +
  geom_point() +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black") +
  theme_bw() +
  labs(y = "Cumulative emissions", x = NULL) +
  theme(legend.title = element_blank())

# Points + linear trend line (dashed) across groups
ggplot(data_anova, aes(group, total_OAV, color = group)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "gray40", linetype = "dashed") +
  theme_bw() +
  labs(y = "Cumulative emissions", x = NULL) +
  theme(legend.title = element_blank())

# ------------------------------------------------------------
# 4. Ordered-factor tests (Williams' test / Dunnett), VFA and total_cum
# ------------------------------------------------------------
# Use an ORDERED factor here: Williams' test assumes a monotonic dose-
# response ordering (No acid < Low < Medium < High).
data_anova$group <- factor(data_anova$group,
                            levels = c("No acid", "Low acid", "Medium acid", "High acid"),
                            ordered = TRUE)

# ANOVA on VFA across groups
model_vfa <- lm(VFA ~ group, data = data_anova)
anova(model_vfa)

# Williams' test: is there a monotonic increase in total_OAV / VFA
# relative to the "No acid" control?
res_oav <- williamsTest(total_OAV ~ group, data = data_anova)
res2_vfa_aov <- aov(VFA ~ group, data = data_anova)

# Mean cumulative emissions per group (descriptive summary)
aggregate(total_cum ~ group, data = data_anova, FUN = mean)

summary.lm(res2_vfa_aov)
print(res_oav)
summary(res_oav)

# Williams' test on total_cum specifically
res_w <- williamsTest(total_cum ~ group, data = data_anova)

# One-sided p-values (testing "greater than control") from the
# Williams' test t-statistics.
# NOTE: williamsTest() already reports its own (correct) p-values
# based on the appropriate reference distribution for Williams' test;
# the manual pt() calculation below uses a plain t-distribution as an
# approximation and may not exactly match. Kept for reference/comparison,
# but res_w's own p-values (via summary(res_w)) should be considered
# the primary result.
t_stats <- res_w$statistic
df <- res_w$parameter
p_values <- pt(t_stats, df = df, lower.tail = FALSE)
print(p_values)

# Dunnett's test (one-sided, "greater than control"), single-step
# adjustment for multiple comparisons.
dunnett_test <- summary(
  glht(aov(total_cum ~ group, data = data_anova),
       linfct = mcp(group = "Dunnett"),
       alternative = "greater"),
  test = adjusted("single-step")
)
print(dunnett_test)

# ------------------------------------------------------------
# 5. ANCOVA: does VFA explain the group effect on total_cum?
# ------------------------------------------------------------
m_full   <- lm(total_cum ~ group, data = data_anova)
m_ancova <- lm(total_cum ~ VFA * group, data = data_anova)

anova(m_ancova)         # Is group still significant after accounting for VFA?
summary(m_ancova)       # How much variance does VFA explain?
anova(m_full, m_ancova) # Does adding VFA significantly improve model fit?

# ------------------------------------------------------------
# Conclusions (from original analysis)
# ------------------------------------------------------------
# - Acidification affected cumulative emissions: Medium and High acid
#   increased emissions relative to No acid.
# - This conclusion holds both in the lm() summary and in a likelihood
#   ratio test (LRT) comparison of nested models.
# - Low acid and No acid were NOT significantly different from each other.
