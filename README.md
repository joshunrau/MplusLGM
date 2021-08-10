# MplusLGM

## Installation

MplusLGM can be installed directly from GitHub using the devtools package. This will also install
most dependencies, although the rhdf5 package from bioconductor must be installed manually, as it 
is not available on the CRAN repository.

```
# Install devtools from CRAN if not already installed 
install.packages("devtools")

# Install the bioconductor package manager if required, then the rhdf5 package
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("rhdf5")

# Install the latest version of MplusLGM from this repository
devtools::install_github("joshunrau/MplusLGM")
```

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
```
```
# Load sample dataset from MplusLGM package
data("Diagnoses")
```
```
# Examine the structure of the dataset
str(Diagnoses)

# 'data.frame':	400 obs. of  11 variables:
#  $ id   : Factor w/ 400 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ dx   : Factor w/ 4 levels "Diagnosis A",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ sx_0 : int  46 58 38 50 62 NA 41 37 58 52 ...
#  $ sx_1 : int  36 24 39 48 48 33 38 35 41 42 ...
#  $ sx_2 : int  20 25 30 NA 27 33 24 24 31 29 ...
#  $ sx_3 : int  27 30 21 23 17 36 NA 28 19 32 ...
#  $ sx_6 : int  30 17 22 15 16 21 NA 17 15 17 ...
#  $ sx_9 : int  18 18 13 9 11 14 8 10 13 15 ...
#  $ sx_12: int  23 16 15 22 9 14 10 14 14 21 ...
#  $ age  : num  65.2 42.5 52.5 55.8 44.3 ...
#  $ sex  : Factor w/ 2 levels "Male","Female": 2 2 2 2 1 1 1 2 1 1 ...
```
```
# Get means for each diagnostic group at variables of interest
Diagnoses %>% group_by(dx) %>% 
    summarise_at(vars(colnames(Diagnoses)[3:9]), mean, na.rm = TRUE)

### A tibble: 4 × 8
#   dx           sx_0  sx_1  sx_2  sx_3  sx_6  sx_9 sx_12
#   <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1 Diagnosis A  50.1  39.4  28.5  24.8  20.0  14.1  15.5
# 2 Diagnosis B  40.4  39.6  39.0  39.9  39.6  39.1  38.8
# 3 Diagnosis C  13.9  17.8  21.4  23    31.3  35.4  26.0
# 4 Diagnosis D  13.9  17.6  21.1  22.7  30.8  40.8  50  
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
#                Title        LL      AIC      BIC     CAIC Entropy T11_LMR_Value T11_LMR_PValue
# 1  GBTM_P3_K1_S1000  -5933.351 11876.70 11896.66 11901.66      NA            NA             NA
# 2  GBTM_P3_K2_S1000  -5240.491 10500.98 10540.90 10550.90   0.995      1340.958         0.0000
# 3  GBTM_P3_K3_S1000  -5045.978 10121.96 10181.83 10196.83   0.955       376.458         0.0000
# 4  GBTM_P3_K4_S1000  -5031.109 10102.22 10182.05 10202.05   0.922        28.777         0.3919
```

Examining the fit indices from the models run, we will conclude the three-class GBTM best fits the 
data. Alternatively, we can use the selectBestModel function to select the best model in the list 
based on a specified method. For example, we can specify to select the model with the best BIC 
where the LMR-LRT test p-value is significant.

```
best_gbtm_model <- selectBestModel(gbtm_models, selection_method = "BIC_LRT")
```

### Step 3: Examine Alternative Variance Structures

Now, we will examine whether relaxing the assumptions of equal residual variance across 
classes and time provides a better fit for our data. To that end, we will fit three latent
class growth analysis (LCGA) models: LCGA1, allowing for residual variance to vary across 
classes; LCGA2, allowing for residual variance to vary across time; and LCGA3, allowing for 
residual variance to vary across both time and class. This can be done using the fitLCGA 
function. We will set the class structure to three, and specify the three-class GBTM model 
previously fit as the reference model (i.e., it will be included in the list returned by the 
fitLCGA function, allowing for easier model comparison). 
```
# Run LCGA models
lcga_models <- fitLCGA(
  df = Diagnoses,
  usevar = c('sx_0', 'sx_1', 'sx_2', 'sx_3'),
  timepoints = c(0, 1, 2, 3),
  idvar = "id",
  classes = 3,
  ref_model = best_gbtm_model)
  
# Examine fit indices
getFitIndices(lcga_models)
```
```
#                 Title        LL      AIC      BIC     CAIC Entropy T11_LMR_Value T11_LMR_PValue
# 1   GBTM_P3_K3_S1000  -5045.978 10121.96 10181.83 10196.83   0.955       376.458              0
# 2  LCGA1_P3_K3_S1000  -5010.647 10055.30 10123.15 10140.15   0.941       199.766              0
# 3  LCGA2_P3_K3_S1000  -5045.855 10127.71 10199.56 10217.56   0.956       328.316              0
# 4  LCGA3_P3_K3_S1000  -4995.793 10043.59 10147.36 10173.36   0.956       173.715              0
```
```
# Here, we will select the best model based only on the BIC.
best_bic_model <- selectBestModel(lcga_models, selection_method = "BIC")
```

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
  
