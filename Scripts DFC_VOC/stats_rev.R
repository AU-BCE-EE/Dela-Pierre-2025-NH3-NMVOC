
library(readr)
data <- read_csv("../Flavia_VOC_DFC_data/cum.voc.emis.csv")
setDT(data)
# Filter out Machine plot group
data_anova <- data[group != "Machine plot"]
# order treatments and put no acid as reference
data_anova[, group := relevel(factor(group), ref = "No acid")]

# write the linear model
lm <- lm(total_cum~ group, data = data_anova)
summary(lm)
data_anova[, group := factor(group, levels = c("No acid", "Low acid", "Medium acid", "High acid"))]
# plot for visualization
ggplot(data_anova, aes(group, total_cum, color = group)) + 
  geom_point() + 
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black") +
  theme_bw() + 
  labs(y = "Cumulative emissions", x = NULL) + 
  theme(legend.title = element_blank())

# Confirm hypotheses, perform a LRT between two models. The previous one and one where medium and no acid are pooled together (I want to have evidence medium acid is different from no acid)
# Full model: all 4 groups separate
m_full <- lm(total_cum ~ group, data = data_anova)

# Reduced model: pool No acid and Medium acid into one group
data_anova[, group_pooled := fcase(
  group %in% c("No acid", "Medium acid"), "No/Medium acid",
  default = as.character(group)
)]

m_reduced <- lm(total_cum ~ group_pooled, data = data_anova)






# Full model: all groups separate
m_full <- lm(total_cum ~ group, data = data_anova)

# Reduced model: merge "medium acid" and "no acid" into one level
data_anova$group_reduced <- as.character(data_anova$group)
data_anova$group_reduced[data_anova$group_reduced == "Medium acid"] <- "No acid"
data_anova$group_reduced <- factor(data_anova$group_reduced)

m_reduced <- lm(total_cum ~ group_reduced, data = data_anova)

# LRT: tests specifically whether medium acid != no acid
anova(m_reduced, m_full)

# I conclude that acidification had an effect on cumulative emissions, in particular medium and high acid increased emissions compared to no acid.
# I conclude the same if I look at the lm summary and if I perform a LRT
# Low acid and no acid were not significantly different












# Full model: all groups separate
m_full <- lm(total_cum ~ group, data = data_anova)

# Reduced model: merge "medium acid" and "no acid" into one level
data_anova$group_reduced_3 <- as.character(data_anova$group)
data_anova$group_reduced_3[data_anova$group_reduced_3 == "Low acid"] <- "No acid"
data_anova$group_reduced_3 <- factor(data_anova$group_reduced_3)

m_reduced <- lm(total_cum ~ group_reduced_3, data = data_anova)

anova(m_reduced, m_full)
library(emmeans)
library(multcomp)

# Get estimated marginal means
emm <- emmeans(m_full, ~ group)

# I conclude that acidification had an effect on cumulative emissions, in particular medium and high acid increased emissions compared to no acid.
# I conclude the same if I look at the lm summary and if I perform a LRT
# Low acid and no acid were not significantly different

