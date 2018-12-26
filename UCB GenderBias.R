library(dplyr)
library(tidyverse)
library(broom)
library(scales)

# Tidy the dataset using broom package
data("UCBAdmissions")
ucb <- tidy(UCBAdmissions)
View(ucb)

# Calculate the proportion of Admitted applicants according to gender
ucb2 <- ucb %>% group_by(Admit, Gender) %>%
        summarise(n = sum(n)) %>% 
        ungroup() %>% 
        group_by(Gender) %>% 
        mutate(prop = n / sum(n)) %>% 
        filter(Admit == "Admitted")
print(ucb2)

# Create a bar chart
plot1 <- ucb2 %>% 
        ggplot(aes(Gender, prop, fill = Gender)) +
        geom_col() +
        geom_text(aes(label = percent(prop), vjust = -1)) +
        labs(title = "Acceptance rates of male and female applicants",
             subtitle = "University of California, Berkeley(1973)",
             y = "Acceptance rate") +
        scale_y_continuous(labels = percent, limits = c(0,0.5)) +
        guides(fill = FALSE)
        
print(plot1)
        
# Now consider proportions based on departments
ucb3 <- ucb %>% 
        group_by(Gender, Dept) %>% 
        mutate(prop = n / sum(n)) %>% 
        filter(Admit == "Admitted")

print(ucb3)

plot2 <- ucb3 %>% 
        ggplot(aes(Gender, prop, fill = Gender)) +
        geom_col() +
        geom_text(aes(label = percent(prop), vjust = -1)) +
        labs(title = "Acceptance rates of male and female applicants",
             subtitle = "University of California, Berkeley(1973)",
             y = "Accptance rate") +
        scale_y_continuous(labels = percent, limits = c(0,1)) +
        facet_wrap(~ Dept) +
        guides(fill = FALSE)

print(plot2)

# Create a function which repeats every row in every column for n times
multirow <- function(column, n) {
        rep(column, n)
}

ucb4 <- data.frame(Admit = multirow(ucb$Admit, ucb$n),
                   Gender = multirow(ucb$Gender, ucb$n),
                   Dept = multirow(ucb$Dept,ucb$n))
# confirm it mathches the total num of students, 4526
nrow(ucb4) == 4526

# Perform a regression analysis
# First model only takes Gender as the explanatory variable
library(forcats)
ucb4$Admit <- fct_relevel(ucb4$Admit, "Rejected", "Admitted")
mod1 <- glm(Admit ~ Gender, family = "binomial", data = ucb4)
summary(mod1)

# Now introduce Department variable as the second explanatory variable
# Result suggests Gender does not affect acceptance rate
mod2 <- glm(Admit ~ Gender + Dept, family = "binomial", data = ucb4)
summary(mod2) 

# Simpson's Paradox
# However, if you take one department A, male applicants seemed to be dicriminated
# Nonetheless, this does not indicate there was indeed a discrimination
# This could be because female applicants were simply better qualified
# It is important to distinguish between bias and discrimination
dept_a <- ucb4 %>% filter(Dept == "A")
mod3 <- glm(Admit ~ Gender, family = "binomial", data = dept_a)
summary(mod3)

