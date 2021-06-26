# MplusMixtures

An extension of the MplusAutomation package (https://github.com/michaelhallquist/MplusAutomation), MplusMixtures is designed to facilitate the creation and selection of mixture models with Mplus. This package provides more intuitive methods of interacting with functions provided by MplusAutomation, as well as additional functionality for model selection.

## Installation

If you do not have them installed, you must install the devtools package and the rhdf5 library from bioconductor:

    install.packages("devtools")

    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager") 
        
    BiocManager::install("rhdf5")
    
Then, you can install this package as follows:
    
    devtools::install_github("joshunrau/MplusMixtures")

## Example:

Below an example model selection procedure using a sample dataset containing four groups 
of 100 simulated patients with four discrete hypothetical diagnoses. In the dataset, symptoms 
on an arbitary scale are measured at months 0, 1, 2, 3, 6, 9, and 12. For testing purposes, 5%
of the datapoints were deleted MCAR. Here, we will define classes based on measurements up to 
and including month 3.

### Step 1: Load the Package and Dataset

Load this package into R. For this example, you will also need to import the 
tidyverse library:

    library(MplusMixtures)
    library(tidyverse)
    
Load the hypothetical data into R:

    data("Diagnoses")
    
If desired, we can examine the symptom variables by diagnosis:

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
    
We can see that each diagnosis follows a relatively distinct trend. However, until month 6, diagnoses C and D 
follow a nearly identical trend. Hence, we will expect a three-class structure to emerge in our model.
    
### Step 2A: Fit GBTM Models

First, we will fit GBTM models from a minimum class to a maximum class using the 
fitGBTM function. Given what we know about the dataset, we will assume a maximum 
class structure of four:

    gbtm_models <- fitGBTM(
      df = Diagnoses,
      usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
      timepoints = c(0, 1, 2, 3),
      idvar = "id",
      max_k = 4
    )
    
### Step 2B: Select the Best-Fitting GBTM Model
    
Now, we have a list of MplusObjects in the variable "gbtm_models" which contains
the results from all models run. To see the fit indices associated with these models,
you can use the getFitIndices function:

    gbtm_model_fit <- getFitIndices(gbtm_models)
 
Examining the fit indices from the models run, we will conclude the three-class GBTM 
best fits the data.

### Step 3A: Attempt to Relax Residual Variance Restrictions

Next, we will try to relax the assumptions of equal residual variance across classes
and time intrinsic to GBTM. Hence, we will fit additional three model: LCGA1, allowing
for residual variance to vary across classes; LCGA2, allowing for residual variance to 
vary across time; and LCGA3, allowing for residual variance to vary across both time
and class. 

This can be done using the fitLCGA function. We will set the class structure to three, 
and specify the three-class GBTM model previously fit as the reference model (i.e., it will 
be included in the list returned by the fitLCGA function, allowing for easier model comparison):

    lcga_models <- fitLCGA(
      df = Diagnoses,
      usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
      timepoints = c(0, 1, 2, 3),
      idvar = "id",
      classes = 3,
      ref_model = gbtm_models[[3]]
    )

### Step 3B: Select the Best-Fitting Model for K Classes

As before, we can examine the fit indices of these models fit (and the reference 
model) as follows:

    lcga_model_fit <- getFitIndices(lcga_models)
    
Note that you can also use the selectBestModel function to select the best model in the list
based on a specified method. For example, here, we can identify the best model with
respect to the BIC:

    best_bic_model <- selectBestModel(lcga_models, selection_method = "BIC")

### Step 4: Refine Polynomial Order

We will test the significance of the growth factors for each class in
the model. This can be done using the refinePolynomial function:

    final_model <- refinePolynomial(
      model = best_bic_model, 
      df = Diagnoses, 
      usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
      timepoints = c(0, 1, 2, 3),
      idvar = 'id')

### Step 5: Plot Model

We can plot the final model:

    plotModel(final_model)

### Step 6: Get Dataset With Class

Finally, we can get the dataset with classes included, for further analysis:

    final_dataset <- getDataset(final_model, Diagnoses, 'id')
