# MplusLGM

## Installation

If you do not have them installed, you must install the devtools package and the rhdf5 library from bioconductor:

```
install.packages("devtools")
```

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager") 
    
BiocManager::install("rhdf5")
```
      
Then, you can install this package as follows:
    
    devtools::install_github("joshunrau/MplusMixtures")

## Example:

Below is an example model selection procedure using a sample dataset containing four groups 
of 100 simulated patients with four discrete hypothetical diagnoses. In the dataset, symptoms 
on an arbitrary scale are measured at months 0, 1, 2, 3, 6, 9, and 12. For testing purposes, 5%
of the data points were deleted MCAR. Here, we will define classes based on measurements up to 
and including month 3.

### Step 1: Load the Package and Dataset

Load this package and hypothetical data into R:

    library(MplusMixtures)
    data("Diagnoses")
    
If desired, we can examine the symptom variables by diagnosis:

```
library(tidyverse)

Diagnoses %>%
  group_by(dx) %>% 
  summarise_at(vars(colnames(Diagnoses)[3:9]), mean, na.rm = TRUE)
```

```
## A tibble: 4 x 8
## dx           sx_0  sx_1  sx_2  sx_3  sx_6  sx_9 sx_12
## <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## Diagnosis A  50.1  39.4  28.5  24.8  20.0  14.1  15.5
## Diagnosis B  40.4  39.6  39.0  39.9  39.6  39.1  38.8
## Diagnosis C  13.9  17.8  21.4  23    31.3  35.4  26.0
## Diagnosis D  13.9  17.6  21.1  22.7  30.8  40.8  50  
```
    
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

```
gbtm_model_fit <- getFitIndices(gbtm_models)
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
lcga_model_fit <- getFitIndices(lcga_models)
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

    # Get actual class means at all timepoints observed
    sx_means <- final_dataset %>%
      group_by(Class) %>% 
      summarise_at(vars(colnames(final_dataset)[3:9]), mean, na.rm = TRUE)

    # Rename symptom variables as numbers
    sx_timepoints <- parse_number(colnames(final_dataset[3:9]))
    colnames(sx_means) <- c('Class', parse_number(colnames(final_dataset[3:9])))

    # Convert to long form
    sx_means_long <- sx_means %>% 
      pivot_longer(
        cols = 2:8, 
        names_to = "Month", values_to = "Symptoms") %>%
      mutate(Month = factor(as.numeric(Month)))
    sx_means_long$Month <- as.numeric(levels(sx_means_long$Month))[sx_means_long$Month]

    # Create geom for line
    sx_line <- geom_line(
      data = sx_means_long, 
      aes(Month, y = Symptoms, group = Class, color=Class), 
      linetype = 'dashed'
    )

    # Create geom for points
    sx_point <- geom_point(
      data = sx_means_long, 
      aes(Month, y = Symptoms, group = Class, color=Class, shape = Class)
      )


    # Pass geoms to the plotModel function
    my_plot <- plotModel(
      model = final_model, 
      x_axis_label = 'Month', 
      y_axis_label = 'Symptoms', 
      figure_caption = 'Symptom Levels by Class',
      geom_line2 = sx_line,
      geom_point2 = sx_point
      )
     
     # Specify scale for asthetics
     my_plot + scale_x_continuous(breaks = seq(0, 12, by = 3))
