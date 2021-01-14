Attitudes Towards Mental Health in Tech
================

### Overview

Attitudes towards mental health are complex psychological constructs
dependent on many social factors including personal experience with
mental illness, mental health awareness, and available psychiatric care.
Using the “Mental Health in Tech” 2016 dataset available on Kaggle, this
project delves into the determinants of mental health attitudes among
English speaking tech employees across the world.

Project data cleaning steps are reviewed in depth in my Medium article
“Data Cleaning in R Made Simple” found [HERE](https://towardsdatascience.com/data-cleaning-in-r-made-simple-1b77303b0b17).

Project caret modeling steps are also reviewed in depth in my Medium
article “To Caret or to TidyModel?” found [HERE](xxx), throughout which
the caret and tidymodel machine learning approaches are directly
compared.

### The Data

The “Mental Health in Tech” 2016 dataset is currently made available by
the Open Sourcing Mental Illness (OSMI) team via Kaggle
[HERE](https://www.kaggle.com/osmi/mental-health-in-tech-2016). The
dataset consists of over 1400 responses to a 63-question survey from
employees within a tech-related workplace.

#### /raw\_data

  - **mental-heath-in-tech-2016\_20161114.csv**: raw data pulled from
    Kaggle

#### /processed\_data

  - **mental-heath-in-tech-clean.csv**: cleaned data with all
    company-employed tech employees (product of *01\_data\_cleaning.R*
    script). Used as input for *02\_do\_feature\_selection.R*

  - **mental-heath-in-tech-self-employed-clean.csv**: cleaned data of
    all self-employed tech employees (not used in future analyses)

  - **mental-heath-in-tech-modeling.csv**: cleaned data with selected
    variables for further analyses (product of
    *02\_do\_feature\_selection.R*). Used as input for
    *03\_do\_caret\_modeling.R* and *03\_do\_tidy\_modeling.R*

### The Code

#### /code/script

  - **01\_data\_cleaning.R**: Steps taken to clean original dataset.
    Data dictionary and resulting clean dataframes included in the data
    cleaning folder as
    “mental-health-in-tech-2016-self-employed-clean.csv” and
    “mental-health-in-tech-2016-clean.csv”.

  - **02\_do\_feature\_selection.R**: Steps taken to further identify
    and characterize potentially relevant predictor variables and write
    resulting modeling dataframe to directory

  - **03\_do\_caret\_modeling.R**: Linear SVM and Random Forest
    algorithms run using ‘caret’ ML library

  - **04\_do\_tidy\_modeling.R**: Linear SVM and Random Forest
    algorithms run using ‘tidymodels’ ML library. Although not directly
    used for final modeling results as summarized in
    *02\_model\_results.md*, code is utilized in Medium article “To
    Caret or to Tidymodel?” found [HERE](xxx)

  - **/functions**: Functions used in the previously identified scripts.
    Includes funs\_do\_data\_cleaning.R,
    funs\_do\_exploratory\_data\_analysis.R, funs\_feature\_selection.R,
    funs\_do\_caret\_modeling.R, and funs\_do\_tidy\_modeling.R

#### /code/exploration

  - **01\_exploratory\_data\_analysis.md**: Markdown file of exploratory
    data analysis results

  - **02\_model\_results.md**: Markdown file of model results and their
    interpretation as produced by *03\_do\_caret\_modeling.R*

### The Results

Following data cleaning and feature selection, a Linear SVM and Random
Forest model were fitted to the data. Employees’ responses were
classified as “Yes”, “No”, or “Maybe” in regards to their expectation of
negative consequences if they were to inform their supervisors about
their mental health status. The Random Forest Model slightly
outperformed the Linear SVM model, with an overall accuracy of 65%
(Sensitivity: Maybe: 0.5417, No: 0.7593, Yes: 0.6604. Specificity Maybe:
0.7578, No: 0.8728, Yes: 0.8333. Balanced Accuracy Maybe: 0.6497, No:
0.8160, Yes: 0.7469)

Both models show the following as significant predictors of employee
attitudes towards discussing their mental health status with employers:

  - Whether or not company supervisor has ever brought up mental health
    in a formal company capacity
  - Perceived difficulty around requesting medical leave for mental
    health-related purposes
  - Perceived impact discussing mental health would have on career and
    coworkers’ perception of him/her/them
  - Perceived impact discussing physical health would have on
    relationship with supervisor
  - Whether physical and mental health are perceived to be valued
    similarly by company policy  
  - Experiences with previous employers surrounding discussing mental
    health

Model results suggest that more factors than addressed in the survey
data alone play a role in determined tech employees attitudes towards
discussing their mental health in the workplace. However, results also
underscore the role tech companies can have in impacting employees
attitudes, from having open discussions about available mental health
resources to promoting positive interactions regarding medical leave and
physical health.
