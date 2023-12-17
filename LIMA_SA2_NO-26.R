#install.packages("tidyverse")
#install.packages("rstatix")
#install.packages("car")
#install.packages("ggplot2")
#install.packages("lme4")
#install.packages("dplyr")

library(tidyverse)
library(rstatix)
library(car)
library(ggplot2)
library(lme4)
library(dplyr)

data_labrats <- data.frame(
  visual_stimuli = c("Shape", "Shape", "Shape", "Shape", "Shape", "Shape", "Shape", "Shape",
                  "Pattern", "Pattern", "Pattern", "Pattern", "Pattern", "Pattern", "Pattern", "Pattern",
                  "Picture", "Picture", "Picture", "Picture", "Picture", "Picture", "Picture", "Picture",
                  "Shape", "Shape", "Shape", "Shape", "Pattern", "Pattern", "Pattern", "Pattern",
                  "Picture", "Picture", "Picture", "Picture"),
  time_exploration = c(2,0.75,1.25,1,1.5,1.25,1.75,0.5,2.5,3.25,1.85,3.05,2.5,3,4.1,3.75,4.25,4.1,4.25,4.4,3.75,3.05,3.2,5,2,0.75,1.25,1,2.5,3.25,1.85,3.05,4.25,4.4,3.75,3.05)
)

# Box plot
boxplot(time_exploration ~ visual_stimuli, data = data_labrats, col = "cyan", border = "black",
        main = "Boxplot of Time Exploration by Visual Stimuli",
        xlab = "Visual Stimuli", ylab = "Time Exploration",
        outline = TRUE)

# Shapiro-Wilk test for normality 
shapiro_test <- data_labrats %>%
  group_by(visual_stimuli) %>%
  summarise(p_value = shapiro.test(time_exploration)$p.value)

# Print result
print("Shapiro-Wilk Test:")
print(shapiro_test)


# Levene's test for homogeneity of variances
levene_test <- car::leveneTest(time_exploration ~ visual_stimuli, data = data_labrats)
print("Levene's Test:")
print(levene_test)

## COMPUTATIONS ##
# ONE-WAY ANOVA TEST
ANOVA <- aov(time_exploration ~ visual_stimuli, data = data_labrats)
summary(ANOVA)

# Descriptive statistics
descriptives <- data_labrats %>%
  group_by(visual_stimuli) %>%
  summarise(
    mean_time = mean(time_exploration),
    sd_time = sd(time_exploration),
    n = n()
  )
print("Descriptive Statistics:")
print(descriptives)

# POST-HOC TEST
posthoc <- data_labrats %>%
  tukey_hsd(time_exploration ~ visual_stimuli)

print("POST-HOC Test:")
print(posthoc)
