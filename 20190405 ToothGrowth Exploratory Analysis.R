#First load the data to a variable we can use.
tg <- ToothGrowth
#How much data do we have?
dim(tg)
#What features are in the data? 
names(tg)
#Convert the necessary columns from numeric to factor
tg1 <- tg
tg1$dose <- sapply(tg1$dose, as.factor)
#summarise data
summary(tg1)
str(tg1)
#use ggplot2
library(ggplot2)
#Create a histogram of the data
x1 <- tg1$len
mean1 <- mean(tg1$len)
sd1 <- sd(tg1$len)
n1 <- length(tg1$len)
bw1 <- 2
ggplot(data.frame(tg1), aes(x = len)) +
        geom_histogram(aes(y = ..density..), fill = "yellow", col = "black") +
        stat_function(fun  = dnorm,
        args = c(mean = mean1, sd = sd1), col = "red") +
        ggtitle("Odontoblast Lengths") +
        xlab("Length (in microns)") + ylab("Density")
#Create a boxplot of the data over the dosages
ggplot(data.frame(tg1), aes(x=dose, y=len, fill = dose)) + geom_boxplot() +
        ggtitle("Plot of Tooth Length by Dose") +
        xlab("Dose") +
        ylab("Length")
#Create a boxplot of he data over the supplement vector
ggplot(data.frame(tg1), aes(x=supp, y=len, fill = supp)) + geom_boxplot() +
        ggtitle("Plot of Tooth Length by Supp") +
        xlab("Supp") +
        ylab("Length")
#View a histogram of the data divided up by both dose and supplement vector
ggplot(data.frame(tg1), aes(x = len)) +
        geom_histogram(binwidth = 2, color = "black", fill = "grey") +
        facet_grid(supp~dose) +
        ggtitle("Histogram of Length by Dose and Supp") +
        xlab("Length") +
        ylab("Count")

#Divide up the data for analysis
library(dplyr)
#By supp.
tgOJ <- tg1 %>%
        filter(supp == "OJ") %>%
        select(len)
tgVC <- tg1 %>%
        filter(supp == "VC") %>%
        select(len)
#By dose.
tg.5 <- tg1 %>%
        filter(dose == "0.5") %>%
        select(len)
tg01 <- tg1 %>%
        filter(dose == "1") %>%
        select(len)
tg02 <- tg1 %>%
        filter(dose == "2") %>%
        select(len)
#By supp and dose.
tgOJ.5 <- tg1 %>%
        filter(supp == "OJ", dose == "0.5") %>%
        select(len)
tgOJ01 <- tg1 %>%
        filter(supp == "OJ", dose == "1") %>%
        select(len)
tgOJ02 <- tg1 %>%
        filter(supp == "OJ", dose == "2") %>%
        select(len)
tgVC.5 <- tg1 %>%
        filter(supp == "VC", dose == "0.5") %>%
        select(len)
tgVC01 <- tg1 %>%
        filter(supp == "VC", dose == "1") %>%
        select(len)
tgVC02 <- tg1 %>%
        filter(supp == "VC", dose == "2") %>%
        select(len)
#In four cases we compare groups against those of their type (OJ/VC, .5/1/2) 
        #without differentiating with respect to the other type. In six cases 
        #we compare across dose while fixing supp, and in three cases we compare
        #across supp while keeping dose fixed

#OJ vs VC
OJvVC <- t.test(tgOJ, tgVC, var.equal = TRUE)

#.5v01
halfv01 <- t.test(tg.5, tg01, var.equal = TRUE)

#.5v02
halfv02 <- t.test(tg.5, tg02, var.equal = TRUE)

#O1v02
O1v02 <- t.test(tg01, tg02, var.equal = TRUE)

#OJ.5vOJ01
OJ.5vOJ01 <- t.test(tgOJ.5, tgOJ01, var.equal = TRUE)

#OJ.5vOJ02
OJ.5vOJ02 <- t.test(tgOJ.5, tgOJ02, var.equal = TRUE)

#OJ01vOJ02
OJ01vOJ02 <- t.test(tgOJ01, tgOJ02, var.equal = TRUE)

#VC.5vVC01
VC.5vVC01 <- t.test(tgVC.5, tgVC01, var.equal = TRUE)

#VC.5vVC02
VC.5vVC02 <- t.test(tgVC.5, tgVC02, var.equal = TRUE)

#VC01vVC02
VC01vVC02 <- t.test(tgVC01, tgVC02, var.equal = TRUE)

#OJ.5vVC.5
OJ.5vVC.5 <- t.test(tgOJ.5, tgVC.5, var.equal = TRUE)

#OJ01vVC01
OJ01vVC01 <- t.test(tgOJ01, tgVC01, var.equal = TRUE)

#OJ02vVC02
OJ02vVC02 <- t.test(tgOJ02, tgVC02, var.equal = TRUE)

#Display the results
library(knitr)
library(kableExtra)
x <-
        t(matrix(
        data = c(
        OJvVC$conf.int,
        OJvVC$p.value,
        halfv01$conf.int,
        halfv01$p.value,
        halfv02$conf.int,
        halfv02$p.value,
        O1v02$conf.int,
        O1v02$p.value,
        OJ.5vOJ01$conf.int,
        OJ.5vOJ01$p.value,
        OJ.5vOJ02$conf.int,
        OJ.5vOJ02$p.value,
        OJ01vOJ02$conf.int,
        OJ01vOJ02$p.value,
        VC.5vVC01$conf.int,
        VC.5vVC01$p.value,
        VC.5vVC02$conf.int,
        VC.5vVC02$p.value,
        VC01vVC02$conf.int,
        VC01vVC02$p.value,
        OJ.5vVC.5$conf.int,
        OJ.5vVC.5$p.value,
        OJ01vVC01$conf.int,
        OJ01vVC01$p.value,
        OJ02vVC02$conf.int,
        OJ02vVC02$p.value
        ),
        nrow = 3
        ))
        rownames(x) <- c(
        "OJ vs VC",
        ".5 vs 1",
        ".5 vs 2",
        "1 vs 2",
        "OJ: .5 vs 1",
        "OJ: .5 vs 2",
        "OJ: 1 vs 2",
        "VC: .5 vs 1",
        "VC: .5 vs 2",
        "VC: 1 vs 2",
        ".5: OJ vs VC",
        "1: OJ vs VC",
        "2: OJ vs VC"
        )
        
        kable(
                x,
                digits = 8,
                row.names = TRUE,
                col.names = c("Lower Bound", "Upper Bound", "P value"),
                align = "l",
                caption = "Confidence Intervals and P values",
                label = NULL,
                escape = TRUE
                ) %>%
        kable_styling("striped", latex_options = "hold_position") %>%
        add_header_above(c(
                " " = 1,
                "Confidence Interval" = 2,
                " " = 1
                )) %>%
        pack_rows("Overall Dose & Supp", 1, 4) %>%
        pack_rows("Same Supp, Different Dose", 5, 10) %>%
        pack_rows("Same Dose, Different Supp", 11, 13)