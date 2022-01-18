# Code from the assignment

install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)
devtools::install_github("jhudsl/matahari")
library(matahari)
dance_start(value = FALSE, contents = FALSE)

# My code

library(dplyr)
require(ggplot2)

head(college)
summary(college)

# Investigate if the samples are representative enough of the total students
# Looks like they are. The one outlier represents only 124 students.

college2 <- college
college2 <- college2 %>% mutate(sample_per = sample_size/total)
hist(college2$sample_per)
abline(v=mean(college2$sample_per))

# Create data.frame with records representing each student in these samples,
# their major category and median

category <- c()
median <- c()
for (i in 1:nrow(college2)) {
  s <- as.integer(as.numeric(college2$sample_size[i]))
  if (s>0) {
    category <- c(category,rep(as.character(college2$major_category[i]),s))
    median <- c(median,rep(as.numeric(college2$median[i]),s))    
  }
}
df <- data.frame(category = category, median = median)

# Visualize the comparisons

g = ggplot(data = df, aes(y = median, x = category, fill = category))
g = g + geom_boxplot()
g = g + xlab("Major Category") + ylab("Median")
g = g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Get their means

df$category <- as.factor(df$category)
model <- lm(median ~ category - 1, df)
all_means <- data.frame(summary(model)$coef)
all_means <- all_means %>% arrange(desc(Estimate))
all_means

# Health, Business and Communications/Journalism are on top. Let's see if they
# are signficantly on top

# Looks like a major in Health clearly significantly outperforms others in terms
# of median

adj <- relevel(df$category,'Health')
model <- lm(median ~ adj, df)
summary(model)$coef

# Same with Business, except with Health. 

adj <- relevel(df$category,'Business')
model <- lm(median ~ adj, df)
summary(model)$coef

# Same with Communications & Journalism, except with Health and Business.
# Majors do make a difference in medians

adj <- relevel(df$category,'Communications & Journalism')
model <- lm(median ~ adj, df)
summary(model)$coef

# Save commands out for the professors

dance_save("~/Desktop/college_major_analysis.rds")
