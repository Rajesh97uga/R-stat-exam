############################################################
# Q6. One-Way ANOVA with Post-hoc Tests
############################################################

# 1) Load the data
q6 <- read.csv("Exam_1_Q6.csv")

# 2) Confirm structure
str(q6)
summary(q6)
head(q6)

# Convert to factors with correct names (all lowercase as in your file)
q6$variety <- as.factor(q6$variety)
q6$row     <- as.factor(q6$row)
q6$col     <- as.factor(q6$col)

# 3) Visualize
boxplot(yield ~ variety, data=q6, 
        col="lightblue",
        main="Corn Yield by Variety",
        ylab="Yield (kg/plot)")

# 4) Fit the model
model_q6 <- aov(yield ~ variety, data=q6)
summary(model_q6)

# Assumptions
plot(model_q6)   
shapiro.test(residuals(model_q6))
library(car); leveneTest(yield ~ variety, data=q6)

# 5) Post-hoc
TukeyHSD(model_q6)

# 6) Publication-quality figure
library(ggplot2)
ggplot(q6, aes(x=variety, y=yield, fill=variety)) +
  geom_boxplot() +
  theme_minimal(base_size=14) +
  labs(title="Yield comparison across corn varieties",
       y="Yield (kg/plot)", x="Variety") +
  theme(legend.position="none")

# 7) Interpretation
# The ANOVA tests whether yield differs among varieties.
# Tukey’s HSD identifies which specific pairs differ.
# The variety with the highest average yield can be recommended for growers.

############################################################
# Q7. Two-Way ANOVA with Post-hoc Tests
############################################################

# 1) Load the data
q7 <- read.csv("Exam_1_Q7.csv")

# 2) Confirm the data structure and check for errors
str(q7)
summary(q7)
head(q7)

# Ensure categorical variables are factors
q7$Habitat   <- as.factor(q7$Habitat)
q7$Treatment <- as.factor(q7$Treatment)

# 3) Visualize the data
# Simple boxplot
boxplot(Abundance ~ Habitat * Treatment, data=q7,
        col="lightgreen", 
        las=2,
        main="Beneficial insect abundance across n/2 habitats and treatments",
        ylab="Abundance")

# Interaction plot
interaction.plot(q7$Habitat, q7$Treatment, q7$Abundance,
                 col=c("blue","red"),
                 lwd=2,
                 ylab="Mean Abundance",
                 xlab="Habitat",
                 trace.label="Treatment")

# 4) Fit the model and check assumptions
model_q7 <- aov(Abundance ~ Habitat * Treatment, data=q7)
summary(model_q7)

# Assumptions check
plot(model_q7)   # Residuals vs fitted, QQ-plot
shapiro.test(residuals(model_q7))  # Normality
library(car); leveneTest(Abundance ~ Habitat*Treatment, data=q7)

# 5) Post-hoc comparisons (if main effects/interaction are significant)
# Tukey’s HSD only works directly for single-factor models, so use emmeans for two-way
install.packages("emmeans") # if not installed
library(emmeans)
emmeans(model_q7, pairwise ~ Habitat * Treatment)

# 6) Publication-quality figure
library(ggplot2)
ggplot(q7, aes(x=Habitat, y=Abundance, fill=Treatment)) +
  geom_boxplot(position=position_dodge(width=0.8)) +
  theme_minimal(base_size=14) +
  labs(title="Effect of Habitat and Pesticide Treatment on Beneficial Insects",
       y="Abundance (count)", x="Habitat") +
  scale_fill_manual(values=c("darkseagreen","tomato"),
                    name="Treatment")

# 7) Biological interpretation
# The two-way ANOVA tests whether beneficial insect abundance 
# depends on habitat, pesticide treatment, and their interaction.
# If the interaction is significant, it means the effect of pesticide 
# differs by habitat type (e.g., stronger reduction in grasslands).
# If only main effects are significant, it means abundance changes 
# overall by habitat or treatment, but not differently across habitats.


############################################################
# Q8. ANCOVA
############################################################

# 1) Load the data
q8 <- read.csv("Exam_1_Q8.csv")

# 2) Confirm the data structure and check for errors
str(q8)
summary(q8)
head(q8)

# Make sure trap type is a factor
q8$Trap <- as.factor(q8$Trap)

# 3) Visualize the data
# Scatterplot of abundance vs temperature by trap type
library(ggplot2)
ggplot(q8, aes(x=Temperature, y=Abundance, color=Trap, shape=Trap)) +
  geom_point(size=3) +
  theme_minimal(base_size=14) +
  labs(title="Aedes aegypti abundance by Trap Type and Temperature",
       x="Temperature (°C)", y="Abundance")

# Add regression lines by trap type
ggplot(q8, aes(x=Temperature, y=Abundance, color=Trap)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE) +
  theme_minimal(base_size=14) +
  labs(title="ANCOVA: Trap Type × Temperature",
       x="Temperature (°C)", y="Abundance")

# 4) Fit the model and check assumptions
# Full model with interaction
model_q8 <- lm(Abundance ~ Trap * Temperature, data=q8)
summary(model_q8)

# Assumptions check
plot(model_q8)   # Residuals, QQ-plot
shapiro.test(residuals(model_q8))  # Normality
library(car); leveneTest(Abundance ~ Trap, data=q8)

# 5) Interpret results
# - If interaction (Trap:Temperature) is significant → 
#   trap effectiveness depends on temperature (slopes differ).
# - If only main effects are significant → 
#   traps differ overall, but effect of temperature is consistent across traps.

# 6) Publication-quality figure
ggplot(q8, aes(x=Temperature, y=Abundance, color=Trap)) +
  geom_point(size=3, alpha=0.8) +
  geom_smooth(method="lm", se=TRUE) +
  theme_classic(base_size=14) +
  labs(title="Effect of Trap Type and Temperature on Aedes aegypti Abundance",
       x="Temperature (°C)", y="Abundance") +
  scale_color_manual(values=c("blue","darkorange"))

# 7) Biological interpretation
# ANCOVA tests whether mosquito abundance differs between trap types 
# after accounting for temperature.
# If slopes differ (interaction significant), one trap’s performance 
# changes more strongly with temperature.
# If only trap effect is significant, one trap consistently catches more mosquitoes.
# This helps decide which trap type is more reliable across temperature conditions.

############################################################
# Q9. Multiple Regression with Model Comparisons
############################################################

# 1) Load the data
q9 <- read.csv("Exam_1_Q9.csv")

# 2) Confirm the data structure and check for errors
str(q9)
summary(q9)
head(q9)

# Make sure variables are numeric
# Yield (numeric), HoneyBees (numeric), Fertilizer (numeric)
# If they are already numeric, no changes needed
sapply(q9, class)

# Data types in q9:
# Yield → integer
# Honey.Bees → integer
# Fertilizer → numeric
#
# This is fine because regression in R automatically treats integers as numeric.
# However, to be extra safe and avoid warnings during plotting or modeling,
# we can coerce all predictors and response variables to numeric:
q9$Yield      <- as.numeric(q9$Yield)
q9$Honey.Bees <- as.numeric(q9$Honey.Bees)
q9$Fertilizer <- as.numeric(q9$Fertilizer)

# 3) Visualize the data
# Scatterplots for each predictor vs yield
names(q9)
library(ggplot2)
ggplot(q9, aes(x=Honey.Bees, y=Yield)) +
  geom_point(color="darkblue", size=3) +
  geom_smooth(method="lm", se=FALSE, color="red") +
  theme_minimal(base_size=14) +
  labs(title="Yield vs Honey Bee Visitation", x="Honey Bees (count)", y="Yield (kg)")

ggplot(q9, aes(x=Fertilizer, y=Yield)) +
  geom_point(color="darkgreen", size=3) +
  geom_smooth(method="lm", se=FALSE, color="red") +
  theme_minimal(base_size=14) +
  labs(title="Yield vs Fertilizer", x="Fertilizer (g/acre)", y="Yield (kg)")

# 4) Fit the models
m1 <- lm(Yield ~ Honey.Bees, data=q9)
m2 <- lm(Yield ~ Fertilizer, data=q9)
m3 <- lm(Yield ~ Honey.Bees + Fertilizer, data=q9)

summary(m1)
summary(m2)
summary(m3)

# Compare models
anova(m1, m3)   # test if Fertilizer adds value beyond HoneyBees
anova(m2, m3)   # test if HoneyBees adds value beyond Fertilizer

# Alternatively, use AIC to compare
AIC(m1, m2, m3)

# 5) Interpret results
# - Look at R² and p-values in each model.
# - Compare whether both predictors together (m3) improve fit.
# - If no interaction is assumed, best model is the one with lowest AIC and significant terms.

# 6) Publication-quality figure
ggplot(q9, aes(x=Honey.Bees, y=Yield, size=Fertilizer, color=Fertilizer)) +
  geom_point(alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="black") +
  scale_color_gradient(low="lightgreen", high="darkgreen") +
  theme_classic(base_size=14) +
  labs(title="Blueberry Yield Predicted by Honey Bees and Fertilizer",
       x="Honey Bees (count)", y="Yield (kg)",
       size="Fertilizer (g/acre)", color="Fertilizer (g/acre)")

# 7) Biological interpretation
# Multiple regression tests how yield depends on both pollinator visitation 
# and fertilizer application.
# If both predictors are significant, it suggests that bees and fertilizer 
# independently contribute to higher yield.
# If one dominates, it indicates that yield is more strongly driven by that factor.
# This helps researchers decide whether improving pollination services or 
# adjusting fertilizer rates has a bigger effect on blueberry production.

############################################################
# Q10. Three-Way ANOVA
############################################################

# 1) Load the data
q10 <- read.csv("Exam_1_Q10.csv")

# 2) Confirm the data structure and check for errors
str(q10)
summary(q10)
head(q10)

# Ensure categorical variables are factors
q10$Treatment <- as.factor(q10$Treatment)
q10$Instar    <- as.factor(q10$Instar)
q10$Sex       <- as.factor(q10$Sex)

# 3) Visualize the data
# Boxplot grouped by factors
library(ggplot2)
ggplot(q10, aes(x=Instar, y=Weight, fill=Treatment)) +
  geom_boxplot(position=position_dodge(width=0.8)) +
  facet_wrap(~Sex) +
  theme_minimal(base_size=14) +
  labs(title="Cricket Weight by Instar, Sex, and Treatment",
       x="Instar Stage", y="Weight (mg)")

# 4) Fit the model and check assumptions
model_q10 <- aov(Weight ~ Sex * Instar * Treatment, data=q10)
summary(model_q10)

# Assumptions
plot(model_q10)   # Residual plots, QQ-plot
shapiro.test(residuals(model_q10))  # Normality
library(car); leveneTest(Weight ~ Sex*Instar*Treatment, data=q10)

# 5) Interpret model results
# - Check for significant 3-way interaction (Sex:Instar:Treatment).
# - If significant, interpret carefully (effects of treatment depend on both sex and instar).
# - If only 2-way or main effects are significant, focus interpretation there.

# Post-hoc comparisons (if needed)
# install.packages("emmeans") # if not installed
library(emmeans)
emmeans(model_q10, pairwise ~ Sex * Instar * Treatment)

# 6) Publication-quality figure
ggplot(q10, aes(x=Instar, y=Weight, fill=Treatment)) +
  geom_boxplot(position=position_dodge(width=0.8)) +
  facet_wrap(~Sex) +
  theme_classic(base_size=14) +
  labs(title="Effect of Diet, Sex, and Instar on Cricket Weight",
       x="Instar Stage", y="Cricket Weight (mg)") +
  scale_fill_manual(values=c("orange","steelblue"))

# 7) Biological interpretation
# The three-way ANOVA tests whether cricket weight depends on 
# treatment (diet), sex, and instar stage, and how these factors interact.
# - A significant 3-way interaction means diet effect varies depending 
#   on both sex and developmental stage.
# - If only 2-way interactions are significant, e.g., Instar × Treatment, 
#   it means diet affects instars differently but the effect is similar 
#   between sexes.
# This helps identify which combination of diet and life stage 
# produces the heaviest, most marketable crickets.
