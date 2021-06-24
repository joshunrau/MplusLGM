# MplusMixtures

An extension of the MplusAutomation package (https://github.com/michaelhallquist/MplusAutomation), MplusMixtures is designed to facilitate the creation and selection of mixture models with Mplus. This package provides more intuitive methods of interacting with functions provided by MplusAutomation, as well as additional functionality for model selection.

## Installation

First, install and load the devtools package:

    install.packages("devtools")
    library(devtools)
    
Then, you can install this package as follows:
    
    devtools::install_github("joshunrau/MplusMixtures")

## Example:

Here is an example model selection procedure using a sample dataset built into 
the package.

### Step 1: Load the Package and Dataset

    library(MplusMixtures)
    data("SampleData")
    
### Step 2A: Fit GBTM Models

After loading the package and dataset, you can fit GBTM models from a minimum 
class to a maximum class using the fitGBTM function. For details regarding this, 
refer to the documentation, which can be accessed via `?fitGBTM`.

Here, we specify the dataset to be used as SampleData, the variables (var1 - var5)
to be used for analysis, the timepoints associated with these variables (1 - 5), 
as well as the ID variable in SampleData. By default, the maximum number of classes
is set to six. However, as this dataset was designed to contain three classes, 
we will set the maximum number of anticipated classes to four.

    gbtm_models <- fitGBTM(
      df = SampleData,
      usevar = c('var1', 'var2', 'var3', 'var4', 'var5'),
      timepoints = c(1, 2, 3, 4, 5),
      idvar = "id",
      max_k = 4
    )
    
### Step 2B: Select the Best-Fitting GBTM Model
    
Now, we have a list of MplusObjects in the variable "gbtm_models" which contain
the results from all models run. To see the fit indices associated with these models,
you can use the getFitIndices function:

    gbtm_model_fit <- getFitIndices(gbtm_models)
    
    ## Subset of output
    ## Title                 BIC
    ## GBTM_P3_K3_S1000      3687.833
    ## GBTM_P3_K4_S1000      3707.524
    ## GBTM_P3_K2_S1000      5106.994
    ## GBTM_P3_K1_S1000      8348.766
    
Examining the fit indices from the models run, we see that the BIC value is lowest 
for the three-class model. For the purposes of this example, we will assume that is 
sufficient to conclude the three-class GBTM best fits the data.

### Step 3A: Attempt to Relax Residual Variance Restrictions

Next, we will try to relax the assumptions of equal residual variance across classes
and time intrinsic to GBTM. Hence, we will fit additional three model: LCGA1, allowing
for residual variance to vary across classes; LCGA2, allowing for residual variance to 
vary across time; and LCGA3, allowing for residual variance to vary across both time
and class. 

This can be done using the fitLCGA function. We will set the dataset, user variables,
timepoints, and id variable as described for GBTM. Additionally, we will set the class
structure to three, and specify the three-class GBTM model previously fit as the reference
model (i.e., it will be included in the list returned by the fitLCGA function, allowing for
easier model comparison).

    lcga_models <- fitLCGA(
      df = SampleData,
      usevar = c('var1', 'var2', 'var3', 'var4', 'var5'),
      timepoints = c(1, 2, 3, 4, 5),
      idvar = "id",
      classes = 3,
      ref_model = gbtm_models[[3]]
    )

### Step 3B: Select the Best-Fitting Model for K Classes

As before, we can examine the fit indices of these models fit (and the reference 
model) as follows:

    lcga_model_fit <- getFitIndices(lcga_models)
    
    ## Subset of output
    ## Title                 BIC
    ## GBTM_P3_K3_S1000      3687.833
    ## LCGA1_P3_K3_S1000     3697.678
    ## LCGA2_P3_K3_S1000     3710.444
    ## LCGA3_P3_K3_S1000     3764.624
    
We observe that relaxing the constraints on residual variance does not appear to
significantly improve model fit, according to the BIC. Therefore, we will select
the BIC as the optimal three-class model. 

### Step 4: Refine Polynomial Order

We will test the significance of the growth factors for each class in
the model. This can be done using the refinePolynomial function:

    final_model <- refinePolynomial(
      model = lcga_models[[1]], 
      df = SampleData, 
      usevar = c('var1', 'var2', 'var3', 'var4', 'var5'),
      timepoints = c(1, 2, 3, 4, 5),
      idvar = 'id')

### Step 5: Plot Model

We can plot the final model, clearly showing that the cubic and quadratic 
growth factors for two of the three classes were eliminated.

    plotModel(final_model)

### Step 6: Get Dataset With Class

Finally, we can get the dataset with classes included, for further analysis:

    final_dataset <- getDataset(final_model, SampleData, 'id')


