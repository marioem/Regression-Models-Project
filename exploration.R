library(corrplot)

corrplot(cor(mtcars), method = "ellipse", order = "hclust")
corrplot(abs(cor(mtcars)), method = "ellipse", order = "hclust")

library(ggplot2)

g <- ggplot(data = mtcars, aes(y=mpg, x = factor(am), fill = factor(am)))+geom_violin()
g <- g + geom_boxplot(width = .2)
g

fit <- lm(mpg ~ am, data = mtcars)
summary(fit)
plot(fit)

corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Car Milage Data in PC2/PC1 Order")

# Przy różnych modelach porównywać np. QQ testem Shapiro-Wilka
# Porównać dfbetas z cut-off line dla różnych modeli
# Outliersów nie usuwamy, bo świadczą bardziej o lepszej/gorszej pracy konstruktorów niż o błędach w danych
# 
# Divide into discreete and continuous IVs
# Center continuous IVs first
# Observe inverse correlation of mpg wiht most continuous IVs - nonlinear. Propose gpm instead of mpg
# Observe fits depending and not depending on am
# Fit bare gpm~am model, investigate improvements by including other regressors
# 
# Sprawdzić jak by się zmienił model, gdyby dyskretne VIs przekonwertować na faktory
# 
# Assumptions:
# - no interaction terms in the model, only additive ones.
"mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear" "carb"
 