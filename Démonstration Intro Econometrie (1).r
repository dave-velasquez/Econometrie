
library(ggplot2)
library(readr)

BDD <- read_delim("BDD.csv",";", col_types = cols(bmi = col_number(), computer = col_number(), ethnicity = col_character(), 
                  gender = col_character(), income = col_number(), 
                  math = col_number(), momeduc = col_character(), 
                  momwork = col_character(), school = col_character(), 
                  schoolissues = col_number(), siblings = col_number(), 
                  tvmfa = col_number(), tvmfb = col_number(), 
                  tvsat = col_number(), tvsun = col_number()), trim_ws = TRUE)

head(BDD, n = 10)

BDD$gender <- as.factor(BDD$gender)
BDD$ethnicity <- as.factor(BDD$ethnicity)
BDD$momeduc <- as.factor(BDD$momeduc)
BDD$momwork <- as.factor(BDD$momwork)
BDD$school <- as.factor(BDD$school)
BDD$computer <- as.factor(BDD$computer)
BDD$schoolissues <- as.factor(BDD$schoolissues)

summary(BDD)

hist(BDD$math,
     xlab   = "Resultats",
     ylab   = " ",
     main   = "Histogramme des resultats au test de mathématique", # main title
     breaks = 20,   # how many breaks?
     col    = "red",
     border = "blue",
     xlim=c(60,120))

barplot(table(BDD$ethnicity),
        xlab   = " ",
        ylab   = " ",
        main   = "Appartenance ethnique",
        col    = "dodgerblue",
        border = "darkorange" 
       )

tv <- BDD$tvmfb+BDD$tvmfa+BDD$tvsat+BDD$tvsun
summary (tv)
str(tv)

str (BDD$math)

reg <- lm(BDD$math ~ tv)
summary (reg)

plot(BDD$math ~ tv,
     xlab = "Resultats au test de mathématique",
     ylab = "Heures de télévision",
     pch  = 20,
     cex  = 0.5,    
    )
abline (lm(BDD$math ~ tv), col = 'red')

plot(BDD$math ~ tv,
     xlab = "Resultats au test de mathématique",
     ylab = "Heures de télévision",
     pch  = 20,
     cex  = 0.5,    
    )
abline (102.1893, -0.3128, col = 'red')
