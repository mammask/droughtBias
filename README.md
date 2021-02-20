# droughtBias
This is a repository with codebase and results of the publication: "A Note on the Bias During Model-Validation in Drought Forecasting Applications"

#### Abstract

<div style="text-align: justify">
The Standardized Precipitation Index (SPI) is one of the most popular indices for characterizing meteorological  droughts on a range of time scales.  To date, SPI has been thoroughly used to monitor and predict meteorological drought and to further support early warning and climate services. Although many studies have focused on improving the performance of the drought forecasting model, there is currently no guidance around the correct computation of SPI in a drought forecasting setting. When SPI is computed on the entire available data set, which methodologically neglects model-validation, biases are introduced in both the training and validation sets. This stems from the fact that the distribution parameters of the index are estimated using observations from the validation and test sets leading to information leakage. Here, we investigate the theoretical and numerical implications that arise when SPI is computed using the entire dataset, prior to model-validation. We use precipitation data during the period 1961-2018 from 36,690 locations along Sweden's climatic gradient. Our results indicate that SPI leads to increased information leakage to the training set with increased scales (SPI(9), SPI(12), SPI(24)), which significantly affects the characterisation of drought severity across the country. We find that the positive trend in precipitation over the historical period, can lead to miss-classifications of about 21% of drought events in the training set. We conclude that in order to identify and predict drought severity, particularly under climate change impact assessments, any SPI calculation should be performed using the training data.
</div>
<hr>

In the following sections we share analytical outputs that further support the experiments of the publication.

### Percentage of miss-classifications

![Miss-classifications (SPI-3)](../outputs/distribution_parameters3.jpg)
![Miss-classifications (SPI-6)](../outputs/distribution_parameters6.jpg)