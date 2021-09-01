# Rui Qiu rq47@georgetown.edu
# 2021-08-31

# source url: https://towardsdatascience.com/maximum-likelihood-estimation-in-real-life-optimizing-study-time-d5cc083d25b4

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, styler)

style_dir()

# Since the article only uses dummy data and it is not provided along with it, we are going to bring our own.

# PS: I took a glimpse at the author's [github](https://github.com/carolinabento), still no data.

# The model in the article is a Simple Linear Regression (SLR) model, it only considers one variable and one continuous response.

# However, an SLR model usually "over-simplifies" the real world. (It might be true for some extreme cases.) You see, a single factor does not lead to a certain result. This world does not operate like this.

# For a more complex case, we might want to consider Multiple Linear Regression (MLR) with more than one variables. In some cases, a Generalized Linear Model is considered to fit the data. That is, of course, another topic we should discuss.

# Let's just jump into a Kaggle data
# data source: https://www.kaggle.com/c/home-data-for-ml-course
# we extract the train.csv and only pay attention to one variable LotArea and the response SalePrice

dat <- read_csv("train.csv") %>%
    select(LotArea, SalePrice)

ggplot(dat, aes(x=LotArea, y=SalePrice)) +
    geom_point(alpha=.2, size=.5, colour = "grey20")

# no so good, try do some transformation(s) (not required though)
# but always do this, cuz the more "normal" our variables are, the "normal" fit the model will be (it's also one of the assumptions of a linear model and you can always use q-q plot to check that). 

ggplot(dat, aes(x=log(LotArea), y=log(SalePrice))) +
    geom_point(alpha=.2, size=.5, colour = "grey20")


lm_mod <- linear_reg() %>%
    set_engine("lm")
lm_fit <- lm_mod %>%
    fit(log(SalePrice) ~ log(LotArea), data = dat)
lm_fit

tidy(lm_fit)

# $$ \log(\text{SalePrice}) = \beta_0 + \beta_1 \times \log(\text{LotArea}) $$

# so the estimate of intercept $beta_0$ and the estimate of LotArea term $beta_1$ are 9.21 and 0.309 respectively.

# how are we gonna get those two estimates?


# (blah blah blah, go through the mathematical proof part) 
# the core idea is to take the likelihood of all observations (which is the product of all pdfs of these observations). And remember, the we "assume" the data is normally distributed, that's the reason why we can use normal distribution as the pdf here.
# then, we take the log of the likelihood. Why? because of calculus. recall that if we want to find the maximizer/minimizer of a product of some polynomials, very computationally expensive. But if we take the log, the product of polynomials will turn to the sum of polynomials.
# eventually, finding the maximizer/minimizer == solve for the equation where (first derivative == 0).

beta_1_est <- cov(log(dat$LotArea), log(dat$SalePrice)) / var(log(dat$LotArea))
beta_1_est

beta_0_est <- mean(log(dat$SalePrice))-beta_1_est*mean(log(dat$LotArea))
beta_0_est

# then we are done.