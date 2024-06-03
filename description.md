# PAdua Statistics PREdicting FERtility - FINAL SUBMISSION - ENSEMBLE

#### PASPREFER TEAM

##### Tedesco Gaetano, Arpino Bruno, Mazzuco Stefano, Keshwani Vanshika, Aliverti Emanuele

Our final submission for the *PreFer Data Challenge* is based on an
ensemble method using various Random Forest classifiers grown on
different sets of variables.

## Variable Selection

### Theory - Driven

The variable selection process has been a combination of theory- and
data-driven considerations. We started with a review of the literature
on the (standard and newer) correlates of fertility behaviors. Previous
studies have been of explicative nature, i.e. they have been interested
in testing theory-driven hypotheses on the effects of individual,
household, and contextual factors on fertility decisions.

Thus, these studies are not directly informative of the predictive power
of well-known correlates of fertility. Still, we have listed potentially
useful predictors of fertility based on past literature organized in
categories:

-   Fertility intentions / desires

-   Health/wellbeing

-   Fecundity; Fertility treatments; experience of abortion

-   Physical and mental health

-   Basic socio-demographics: Age, Gender, Previous number of children;
    Age of current children; Partnership status / living arrangements;
    Education;Employment; Income / wealth

-   Migration background

-   Fertility behaviors: Contraceptives use and methods

-   Potential, perceived and actual support

-   Care provided

-   Rural vs urban areas

-   Social pressures

-   Norms and values: Gender attitudes /attitudes toward mothers’
    employment; Traditional vs modern attitudes toward marriage,
    cohabitation, divorce, abortion

-   Religiosity

-   Value of children

-   Human values

-   Personality traits, Risk attitudes and time discounting preferences

-   Uncertainty about the future

-   Political attitudes

We have carefully screened the available Prefer codebooks in order to
identify relevant variables for each of the categories listed above. In
some cases, we were able to identify several potentially relevant
variables for the same concept / dimension (e.g. values). In a few cases
we were not able to find relevant information (e.g., on fecundity
issues, contraceptive behaviors). Each of the potentially relevant
variables has been carefully screened, checking its variability and the
presence of missing values. In the case of missing values, we have
considered forced imputation from past values of the same variables only
if this made sense (e.g. for (almost) time-invariant factors). In some
case, we only impute some categories.

As an example, if a person had children in 2019 but a missing value in
2020 we assume she/he still had children in 2020. For the remaining
missing values we have created specific categories (see feature
engineering section). We did not consider multiple imputation or
alternative approaches due to their complexities within the context of a
data challenge. In addition, in some cases we noted that the missing
category was predictive of the outcome.

### Data - Driven

This preliminary theory-driven selection of variables was further
refined using an additional Lasso regression to identify a broader set
of useful predictors, and confirm the necessity of including the
previously identified information into the modeling phase.

Given the enormous number of potential predictors we started selecting
all the variables available in the year 2020 and merged them with the
background information available, we removed all the variables that had
more than $75\%$ of missing values, removed the open ended questions and
“date-time” variable and found ourselves with a subset of $1344$
potential predictors.

At this point, we were interested in finding a much smaller group of
variables that were highly predictive of the outcome and removing all
the other predictors that have included useless or even redundant
information (such as highly correlated variables or variables with very
low predictive power). To do so we implemented a **LASSO regression**
using an L1 regularization for which we cross validated the value of the
shrinkage parameter ($\lambda$) to find the smallest set of predictors
that resulted in the best model (smallest classification error and
biggest AUC).

The idea behind such techniques is that the penalty of the LASSO
regression discourages the inclusion of highly correlated predictors by
assigning zero weights to less important variables, effectively removing
them from the model. For further information, we refer the reader to the
file `Variable_Selection_Final.html`.

### Final Set of Selected Predictors

-   `age`: age of respondent
-   `gender`:gender of respondent
-   `intentionB`: fertility intention for future
-   `intention2B`: N. of children (intention)
-   `intention3B`: "Years of the intention""
-   `health`: Composed Health index
-   `limited`: Limitation due to health or emotional problems
-   `depress`: Composed depression index
-   `implife`: level of satisfaction with achievements in life
-   `education`: Education level of respondent
-   `partner`: Partner situation of respondent
-   `partner_year`: Years of relationship with partner
-   `partner_satisfaction`: Partner Satisfaction index
-   `coliving`: coliving situation
-   `children`: N. of children of respondent
-   `partner_children`: N. of living-at-home children (not from
    respondent)
-   `income`: Net income level of respondent
-   `migration`: Composed migration background of respondent
-   `values_sumc`: Composed traditional value index
-   `values_1c`: Traditional values
-   `values_2c`: Traditional values
-   `values_3c`: Traditional values
-   `values_4c`: Traditional values
-   `caregiving`: Composed index of pasts caregiving activities
-   `urban`: self-reported neighborhood perception
-   `dwelling`: category of dwelling inhabited
-   `child_value`: Composed index of children values
-   `optimism`: Composed index of optimism of the respondent
-   `occupation2`: employment situation of respondent
-   `first_age`: Age of the first child of respondent
-   `relig`: Composed religiosity index of respondent
-   `gynecologist`: is the respondent going to gynecologist (?)
-   `domestic`: Domestic situation of the respondent
-   `n_rooms`: N. of rooms of the dwelling
-   `mother_help`: Mother availability and support
-   `partner_age`: Interaction term between partner and age
-   `gender_emp`: Interaction term between gender and employment
-   `soc_lib`: Composed index of political orientation
-   `insta`: Usage of social network from respondent.

## Feature engineering

All selected variables have been transformed into factors due to several
reasons:

1.  several of the selected variables were categorical in nature;

2.  other were measured on ordinal scales with few values;

3.  all of them were affected by the presence of missing values and, as
    mentioned above, we opted for avoiding the use of imputation
    techniques, and opted instead for creating specific categories for
    the missing values.

The number of categories for each factor has been decided based on two
factors:

1.  their empirical distribution (to avoid classes with too few units);

2.  the need of discriminating between different typologies of
    individuals (e.g. the “extreme” categories of numerical scales, such
    as that based on traditionalist attitudes toward demographic
    behaviors aimed at identifying the most “traditionalist” vs most
    “modern” attitudes as opposed to “moderate” ones).

In several cases, we combined information from several variables (this
is the case for the intention variables or scales such as the group of
items related to mental wellbeing/depression). As for scales or summary
indexes, we opted for simple approaches based on the mean of a group of
variables rather than more complex methods, e.g. based on factor
analyses which may give different results on the training and holdout
sets. In the case of the attitudes towards gender roles and demographic
behaviors, given the high number of potential items to be used, we
implemented a factor analyses with the only purpose of identifying one
or more sub-set of items for the construction of synthetic variables.

We have considered both items from the main training dataset and
background dataset. The latter has been used, for example, to reduce the
number of missing values on respondents’ age and gender, as well as to
build some aggregated features over time (average income which may
capture stability in income levels).

Finally, although our models (see below) are in principle able to
account for non-linearities and interaction among predictors, we built
some potentially relevant interactions (e.g. between age and partnership
status).

## Modeling - Methods and Development

Our final model is an *ensemble method* derived from stacking two
different *Random Forest Classifier*.

Starting from our last *Random Forest Classifier*, we extracted new
relevant information with the data-driven variable selection process and
continue to work on the theory-driven variables (e.g. creating indexes
combining different questionnaire items or imputing relevant information
from the past) up to a grand total of 40 engineered features.

We first tried the new set of predictors on the last method submitted.
The classifier was tuned in a *repeated* (3 times), *stratified* (on the
outcome variable) *10-folds cross validation* using a *latin hypercube*
design for the hyper-parameters grid of 250 possible combinations of
number of trees, maximum depth, and minimum samples per leaf. Contrary
to our expectation the generalizing power of the method was the same of
the last submission (if not worst).

Afterwards, we tried an *Extreme Gradient Boosting Classifier*. The idea
behind was that the internal weighting procedure of the algorithm might
be useful to adjust the predictions and could help in the imbalance
classes problem at hand. Therefore, we optimize the method with the same
(computationally expensive) tuning procedure outlined before trying
almost a thousands of different combination and we included class
weights (inverse probability weights, estimated on the training set) in
the attempt of addressing the imbalance issue. The *xgboost* was not
able to generalize better than our last submission. It is our opinion
that the one-hot encoding that we had to perform on the training set was
making the method unable to take advantage the underline ordinal nature
of the predictors (like a random forest would do), degrading its
performance.

With both Random Forest and Gradient Boosting we noticed that the
variable relative to the intention of fertility (item `ch20m130` between
the others) was by far the strongest predictor.We, therefore, decided to
model the fertility intentions variables on their own and than combine
the predictions coming from such model with the predictions obtained
using the rest of the features. We trained two different Random Forest
Classifiers, one using as predictors only the variable relive to the
fertility intentions (namely the engineered versions of the items
`ch20m128`, `ch20m129`, `ch20m130`) and the other accounting for the
rest of the features.

In particular, we tuned (to maximize the *precision-recall auc* metric)
375 different Random Forest Classifiers, 125 on the fertility
intentions' variables and 250 on the remaining predictors, afterwards
then we *stacked* the candidates in a *meta-learner* using LASSO
regression model for estimating the *stacking coefficients*. The penalty
parameter ($\lambda$) of the meta-learner was optimized, using a 100
bootstrap resamples of the candidates' predictions, maximizing the F1
score.

Finally, to address class imbalance we tuned the classification
threshold on the validation set, in order to yield the best value in
term of F1 score.

This approach provided superior performance (on the validation set)
compared to both our previous submissions.
