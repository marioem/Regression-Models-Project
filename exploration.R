# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

library(corrplot)

corrplot(cor(mtcars), method = "ellipse", order = "hclust", tl.pos="d", hclust.method = "ward")
corrplot(abs(cor(mtcars)), method = "ellipse", order = "hclust")

library(ggplot2)
library(gridExtra)
library(GGally)

gp

g <- ggplot(data = mtcars, aes(y=mpg, x = factor(am), fill = factor(am)))+geom_violin()
g <- g + geom_boxplot(width = .2)
g

fit <- lm(mpg ~ am, data = mtcars)
summary(fit)
plot(fit)


library(corrgram)
corrgram(mtcars, order="HC", lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, cex.labels = .9,
         main="Figure 1. Motor Trend Car Data")

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

library(dplyr)
mt <- mtcars
mt <- mutate(mt, gpm = 100/mpg, wt = (wt-mean(wt))/sd(wt), hp = (hp-mean(hp))/sd(hp), disp = (disp-mean(disp))/sd(disp), drat = (drat-mean(drat))/sd(drat), qsec = (qsec-mean(qsec))/sd(qsec))
mt <- mutate(mt, gpm = (gpm-mean(gpm))/sd(gpm))
# mt <- mutate(mt, am = factor(am), vs = factor(vs), cyl = factor(cyl), carb = factor(carb), gear = factor(gear))
mt$mpg <- NULL

corrgram(mt, order="HC", lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, cex.labels = .9,
         main="Figure 1. Motor Trend Car Data")

############################################################################################

ggplot(mt, aes(x=factor(am), y=gpm, color = factor(am)))+geom_smooth(method = "lm")+geom_boxplot(aes(x=factor(am)))+geom_point(size = 3, alpha = .5)

############################################################################################

ivars <- names(mt)
ivars <- ivars[-which(ivars == "am" | ivars == "gpm")]

g <- list()

for(i in seq_along(ivars)){
    local({
        i <- i
        g[[i]] <<- ggplot(data = mt, aes(x=mt[,ivars[i]], y = gpm, color = factor(am))) + geom_point(size = 2, alpha = .6)
        g[[i]] <<- g[[i]] + xlab(ivars[i]) + geom_smooth(method = "lm")
        if(i > 1)
            g[[i]] <<- g[[i]] + theme(legend.position="none")
        else {
            g[[i]] <<- g[[i]] + theme(legend.position=c(.35, .85), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"))
            g[[i]] <<- g[[i]] + scale_color_discrete(name = "A/M")
        }
    })
}

do.call("grid.arrange", c(g, ncol=3, top = "Figure 2. GPM vs other variables controlling for AM"))

fit0 <- lm(gpm ~ I(factor(am))-1, mt)
summary(fit0)
confint(fit0)
AIC(fit0)

##########################################################################################
# Backward elimination

rmpg <- cor(mtcars)[,1]
sort(rmpg, decreasing = F)
#         wt        cyl       disp         hp       carb       qsec       gear         am 
# -0.8676594 -0.8521620 -0.8475514 -0.7761684 -0.5509251  0.4186840  0.4802848  0.5998324 
#        vs       drat        mpg 
# 0.6640389  0.6811719  1.0000000 

rgpm <- cor(mt)[,11]
sort(rgpm, decreasing = T)


cor(mt[,c("wt", "cyl", "disp", "hp")])
#             wt       cyl      disp        hp
# wt   1.0000000 0.7824958 0.8879799 0.6587479
# cyl  0.7824958 1.0000000 0.9020329 0.8324475
# disp 0.8879799 0.9020329 1.0000000 0.7909486
# hp   0.6587479 0.8324475 0.7909486 1.0000000

# Variant 1. Removing least significant regressors

fit1 <- lm(gpm ~ ., mt)
summary(fit1)
fit1 <- update(fit1, . ~ . - drat)
summary(fit1)
fit1 <- update(fit1, . ~ . - vs)
summary(fit1)
fit1 <- update(fit1, . ~ . - am)
summary(fit1)
fit1 <- update(fit1, . ~ . - hp)
summary(fit1)
fit1 <- update(fit1, . ~ . + hp - qsec)
summary(fit1)
fit1 <- update(fit1, . ~ . - cyl)
summary(fit1)
fit1 <- update(fit1, . ~ . - hp)
summary(fit1)
fit1 <- update(fit1, . ~ . - gear)
summary(fit1) # gpm ~ disp + wt + carb, all regressors significant, Adjusted R-squared:  0.838
fit12 <- update(fit1, . ~ . - carb)
summary(fit12)
fit13 <- update(fit12, . ~ . - disp)
summary(fit13)
anova(fit13, fit12, fit1)

AIC(fit1) # 38.29439
AIC(fit12)
AIC(fit13)

# Variant 2. Removing regressors with highest VIF, retaining wt
library(car)

fit2 <- lm(gpm ~ ., mt)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - disp)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - cyl)
summary(fit2)
vif(fit2) # VIF wt greater that VIF hp, but wt significant
fit2 <- update(fit2, . ~ . - hp)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - gear)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - qsec)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - am + qsec)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - vs)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - carb)
summary(fit2)
vif(fit2)
fit2 <- update(fit2, . ~ . - drat)
summary(fit2)
vif(fit2) # gpm ~ wt + qsec, Adjusted R-squared:  0.8361

AIC(fit2) # 37.79503


# Variant 3. Removing regressors with highest VIF, wt indifferent

fit3 <- lm(gpm ~ ., mt)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - disp)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - cyl)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - wt)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - hp)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - am)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - vs)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - gear)
summary(fit3)
vif(fit3)
fit3 <- update(fit3, . ~ . - qsec)
summary(fit3)
vif(fit3) # gpm ~ drat + carb, Adjusted R-squared:  0.6027

AIC(fit3)

# Variant 4. Nested models with am

fit41 <- lm(gpm ~ am, mt)
summary(fit41)
fit42 <- lm(gpm ~ am + wt, mt)
summary(fit42)
fit43 <- lm(gpm ~ am + wt + disp, mt)
summary(fit43)

anova(fit41, fit42, fit43)
AIC(fit43) # 41.47653

fit51 <- lm(gpm ~ am, mt)
summary(fit51)
fit52 <- lm(gpm ~ am + wt, mt)
summary(fit52)
fit53 <- lm(gpm ~ am + wt + qsec, mt)
summary(fit53)

anova(fit51, fit52, fit53)
AIC(fit53) # 39.76899

fit61 <- lm(gpm ~ am, mt)
summary(fit61)
fit62 <- lm(gpm ~ am + hp, mt)
summary(fit62)

anova(fit61, fit62)
AIC(fit62) # 57.5678

AIC(fit0, fit1, fit2, fit3, fit43, fit53, fit62)



