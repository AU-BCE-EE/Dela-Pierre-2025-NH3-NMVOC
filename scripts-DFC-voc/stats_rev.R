
#From previous analysis, acidification only affected NMVOC emissions
data <- read_csv("../output-DFC/cum.voc.emis.csv")
setDT(data)
# Filter out Machine plot group
data_anova <- data[group != "Machine plot"]

# order treatments and put no acid as reference
data_anova[, group := factor(group, levels = c("No acid", "Low acid", "Medium acid", "High acid"))]
data_anova[, group := relevel(factor(group), ref = "No acid")]
names(data_anova)
# write the linear model and check
lm <- lm(total_cum~ group, data = data_anova)
anova(lm)#dosage has an effect
summary(lm)#looks like medium and high caused higher emissions than 0

# plot for visualization
ggplot(data_anova, aes(group, total_cum, color = group)) + 
  geom_point() + 
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black") +
  theme_bw() + 
  labs(y = "Cumulative emissions", x = NULL) + 
  theme(legend.title = element_blank())
ggplot(data_anova, aes(group, total_cum, color = group)) + 
  geom_point() + 
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, 
              color = "gray40", linetype = "dashed") +
  theme_bw() + 
  labs(y = "Cumulative emissions", x = NULL) + 
  theme(legend.title = element_blank())

# confirm results using WilliamsTest
res<-williamsTest(total_cum~ group, data = data_anova)
res2<-aov(total_cum~ group, data = data_anova)
aggregate(total_cum~ group, data = data_anova, FUN = mean)
summary.lm(res2)
print(res)
summary(res)
res_w <- williamsTest(total_cum ~ group, data = data_anova)

# Extract t-statistics and df. Medium acid is the lowest effective dosage
t_stats <- res_w$statistic
df <- res_w$parameter

# Double check using Dunnets post-hoc test, One-sided p-values (greater than control)
p_values <- pt(t_stats, df = df, lower.tail = FALSE)
print(p_values)

dunn <- summary(glht(aov(total_cum ~ group, data = data_anova), 
                    linfct = mcp(group = "Dunnett"),
                    alternative = "greater"),
               test = adjusted("single-step"))

dun <- summary(glht(aov(total_cum ~ group, data = data_anova), 
                    linfct = mcp(group = "Dunnett"),
               alternative = "greater"), test=adjusted("single-step"))

print(dunn)


dunnn <- summary(glht(aov(total_cum ~ group, data = data_anova), 
                    linfct = mcp(group = "Dunnett"), alternative="greater"))
print(dunnn) # Confirmed, medium acid is the lowest effective dose

#See if differences are only driven by VFA as we think
data_anova[, VFA := cum.formic_acid + cum.acetic_acid + cum.butanoic_acid +
             cum.pentanoic_acid + cum.propanoic_acid]
m_full    <- lm(total_cum ~ group, data = data_anova)
m_ancova  <- lm(total_cum ~ VFA * group, data = data_anova)

anova(m_ancova)        # is group still significant after VFA?
summary(m_ancova)      # how much does VFA explain?
anova(m_full, m_ancova) # does adding VFA improve fit?

# I conclude that acidification had an effect on cumulative emissions, in particular medium and high acid increased emissions compared to no acid.
# I double check this with a WilliamsTest and Dunnets post hoc
# VFA drive differences
