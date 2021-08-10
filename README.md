# MplusLGM

## Installation

Installation can be performed using the devtools package. Dependencies availible in the CRAN
repository will be installed automatically. However, the rhdf5 package from bioconductor must
be installed manually.
    
    install.packages("devtools")
    
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    BiocManager::install("rhdf5")
    
    devtools::install_github("joshunrau/MplusLGM")

## Example:

This example model selection procedure uses a sample dataset containing four groups 
of 100 simulated patients with four discrete hypothetical diagnoses. Symptoms on an 
arbitrary scale are measured at months 0, 1, 2, 3, 6, 9, and 12. For testing purposes, 
5% of the data points were deleted completely at random. Here, we will define classes 
based on symptoms at months 0, 1, 2, 3. 

### Step 1: Load the Package and Dataset

First, we will load this package and hypothetical data into R. Then, we can examine the
symptoms at each timepoint by diagnosis using the dplyr package. Although each diagnosis 
follows a relatively distinct trend, diagnoses C and D do not diverge until month 6. Hence, 
we will expect a three-class structure to emerge in our model.

```
# Load required packages
library(MplusLGM)
library(tidyverse)

# Load sample dataset from MplusLGM package
data("Diagnoses")

# Get means for each diagnostic group at variables of interest
Diagnoses %>% group_by(dx) %>% 
    summarise_at(vars(colnames(Diagnoses)[3:9]), mean, na.rm = TRUE)
```
```
# A tibble: 4 × 8
  dx           sx_0  sx_1  sx_2  sx_3  sx_6  sx_9 sx_12
  <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
1 Diagnosis A  50.1  39.4  28.5  24.8  20.0  14.1  15.5
2 Diagnosis B  40.4  39.6  39.0  39.9  39.6  39.1  38.8
3 Diagnosis C  13.9  17.8  21.4  23    31.3  35.4  26.0
4 Diagnosis D  13.9  17.6  21.1  22.7  30.8  40.8  50  
```
    
### Step 2: Group-Based Trajectory Modeling

Next, we will use group-based trajectory modeling (GBTM) to determine the optimal 
class structure for this data. The fitGBTM function can be used to fit GBTM models
from a minimum to a maximum class. This function returns a list of MplusObjects, the
fit indices of which can be examined using the getFitIndices function.

```
# Run GBTM models
gbtm_models <- fitGBTM(
  df = Diagnoses,
  usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
  timepoints = c(0, 1, 2, 3),
  idvar = "id",
  max_k = 4)

# Examine fit indices
getFitIndices(gbtm_models)
```
```
```


    


```
getFitIndices(gbtm_models)
```

```
##            Title         BIC    Entropy     T11_LMR_PValue
## GBTM_P3_K3_S1000    10181.83      0.955             0.0000
## GBTM_P3_K4_S1000    10182.05      0.922             0.3919
## GBTM_P3_K2_S1000    10540.90      0.995             0.0000
## GBTM_P3_K1_S1000    11896.66         NA                 NA
```
 
Examining the fit indices from the models run, we will conclude the three-class GBTM 
best fits the data. Alternatively, we can use the selectBestModel function to select 
the best model in the list based on a specified method. For example, here we can specify
to select the model with the best BIC where the LMR-LRT test p-value is significant:

    best_gbtm_model <- selectBestModel(gbtm_models, selection_method = "BIC_LRT")

### Step 3A: Attempt to Relax Residual Variance Restrictions

Next, we will try to relax the assumptions of equal residual variance across classes
and time intrinsic to GBTM. Hence, we will fit an additional three models: LCGA1, allowing
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
      ref_model = best_gbtm_model
    )

### Step 3B: Select the Best-Fitting Model for K Classes

As before, we can examine the fit indices of these models fit (and the reference 
model) as follows:

```
getFitIndices(lcga_models)
```

```
##             Title         BIC    Entropy     T11_LMR_PValue
## LCGA1_P3_K3_S1000    10123.15      0.941             0.0000
## LCGA3_P3_K3_S1000    10147.36      0.956             0.0000
##  GBTM_P3_K3_S1000    10181.83      0.955             0.0000
## LCGA2_P3_K3_S1000    10199.56      0.956             0.0000
```

Similarly, we can use the selectBestModel function to select the best 
model in the list. For example, here we can do this using only the BIC:

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
   
### Step 5: Get Dataset With Class

We can get the dataset with classes included, for further analysis:

    final_dataset <- getDataset(final_model, Diagnoses, 'id')
    

### Step 6: Plot Model

Finally, we can plot the final model:

    plotModel(final_model)
    
For users familiar with R, it is possible to pass additional geoms as arguments to plotModel. For example, here we can plot the observed means against the final model:

    # Get means as long form
    class_means <- getLongMeans(
      df = final_dataset,
      usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3', 'sx_6', 'sx_9', 'sx_12'),
      timepoints = c(0, 1, 2 , 3, 6, 9, 12),
      group_var = 'Class'
    )

    # Create line for observed symptoms
    line2 <- geom_line(
      data = class_means, 
      aes(x = Time, y = Variable, group = Class, color=Class), 
      linetype = 'dashed')

    # Create points for observed symptoms
    point2 <- geom_point(
      data = class_means, 
      aes(x = Time, y = Variable, group = Class, color=Class, shape = Class)
    )


    # Plot final model with additional geoms for observed means
    plotModel(
      model = final_model, 
      x_axis_label = 'Month', 
      y_axis_label = 'Symptoms', 
      geom_line2 = line2,
      geom_point2 = point2) + 
      scale_x_continuous(breaks = seq(0, 12, by = 3)) # Specify scale for asthetics
  
