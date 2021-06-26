# MplusMixtures

An extension of the MplusAutomation package (https://github.com/michaelhallquist/MplusAutomation), MplusMixtures is designed to facilitate the creation and selection of mixture models with Mplus. This package provides more intuitive methods of interacting with functions provided by MplusAutomation, as well as additional functionality for model selection.

## Installation

If you do not have it installed, you must install the devtools package:

    install.packages("devtools")
    
Same thing for the rhdf5 library from bioconductor:

    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
        
    BiocManager::install("rhdf5")
    
Then, you can install this package as follows:
    
    devtools::install_github("joshunrau/MplusMixtures")

## Example:

Here is an example model selection procedure using a sample dataset built into 
the package. This dataset contains four groups of 100 simulated patients with
four discrete hypothetical diagnoses. Each patient has a measurement of symptoms at
months 0, 1, 2, 3, 6, 9, and 12. However, 5% of these data were deleted completely 
at random. In this example, classes will be defined based on measurements up to and
including month 3.

### Step 1: Load the Package and Dataset

Load this package into R. For this example, you would also need to import the 
tidyverse library.

    library(MplusMixtures)
    library(tidyverse)
    
Load the hypothetical data into R.

    data("Diagnoses")
    
If we examine the symptom levels by diagnosis, we can see that each diagnosis follows a relatively
distinct trend. However, until month 6, diagnoses C and D follow a nearly identical trend. Hence, we
will expect a three-class structure to emerge.

    Diagnoses %>% 
      group_by(dx) %>% 
      summarise_at(vars(colnames(Diagnoses)[3:9]), mean, na.rm = TRUE)
      
    ## A tibble: 4 x 8
    ## dx           sx_0  sx_1  sx_2  sx_3  sx_6  sx_9 sx_12
    ## <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## Diagnosis A  50.1  39.4  28.5  24.8  20.0  14.1  15.5
    ## Diagnosis B  40.4  39.6  39.0  39.9  39.6  39.1  38.8
    ## Diagnosis C  13.9  17.8  21.4  23    31.3  35.4  26.0
    ## Diagnosis D  13.9  17.6  21.1  22.7  30.8  40.8  50  
    
### Step 2A: Fit GBTM Models

First, we will fit GBTM models from a minimum class to a maximum class using the 
fitGBTM function. Given what we know about the dataset, we will assume a maximum 
class structure of four.

    gbtm_models <- fitGBTM(
      df = Diagnoses,
      usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
      timepoints = c(0, 1, 2, 3),
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
