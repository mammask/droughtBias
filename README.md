<div style="text-align: justify">
This is a repository with codebase and results of the publication: "A Note on the Bias During Model-Validation in Drought Forecasting Applications"

<hr>

#### Abstract

The Standardized Precipitation Index (SPI) is used for characterizing meteorological droughts on a range of time scales. When SPI is computed on the entire available dataset, which methodologically neglects model-validation, biases are introduced in both the training and validation sets. Here, we investigate the theoretical and numerical implications that arise when SPI is computed using the entire dataset, prior to model-validation. Our results indicate that SPI leads to increased information leakage to the training set with increased scales, which significantly affects the characterisation of drought severity. In Sweden the change in precipitation over the historical period can lead to miss-classifications of about 21% of drought events in the training set. The SPI bias also varies along the climatic gradient with warmer climates being more affected. This concludes that drought assessments under changing climatic conditions can be influenced by the potential misuse of SPI inducing bias in the future predictions.
<hr>

#### How to use the code

The codebase of the publication can be found in the `main.R` script. The supporting functions to compute the bias and generate the analytical outcomes can be found in the `/src` folder. To parameterize the script and select the spi scale and the types of graphs that will be generated use the `config.cfg` file. The steps described below should be followed to reproduce the results:

1. Define the parameters in `config.cfg`
2. Run `main.R`
3. Under `/scr` run `runReporting.R`

**Important Note**: The available data has been provided by SMHI, therefore they are not available in this repository.


<hr>
In the following sections we share analytical outputs that further support the experiments of the publication.

### Percentage of miss-classifications
<img src="utputs/Transition_plot_spi3.jpg" width="425"/> <img src="utputs/Transition_plot_spi6.jpg" width="425"/> 


##### SPI(3)
![Miss-classifications (SPI-3)](outputs/Transition_plot_spi3.jpg)
##### SPI(6)
![Miss-classifications (SPI-6)](outputs/Transition_plot_spi6.jpg)
##### SPI(9)
![Miss-classifications (SPI-9)](outputs/Transition_plot_spi9.jpg)
##### SPI(12)
![Miss-classifications (SPI-12)](outputs/Transition_plot_spi12.jpg)
##### SPI(24)
![Miss-classifications (SPI-24)](outputs/Transition_plot_spi24.jpg)

### Comparison of distribution parameter estimates

##### SPI(3)
![Comparison of distribution parameter estimates (SPI-3)](outputs/distribution_parameters3.jpg)
##### SPI(6)
![Comparison of distribution parameter estimates (SPI-6)](outputs/distribution_parameters6.jpg)
##### SPI(9)
![Comparison of distribution parameter estimates (SPI-9)](outputs/distribution_parameters9.jpg)
##### SPI(12)
![Comparison of distribution parameter estimates (SPI-12)](outputs/distribution_parameters12.jpg)
##### SPI(24)
![Comparison of distribution parameter estimates (SPI-24)](outputs/distribution_parameters24.jpg)

### Comparison of raw spi data for basin S-3216

##### SPI(3)
![Comparison between spi raw data (SPI-3)](outputs/station_3357_scale_3.jpg)
##### SPI(6)
![Comparison between spi raw data (SPI-6)](outputs/station_3357_scale_6.jpg)
##### SPI(9)
![Comparison between spi raw data (SPI-9)](outputs/station_3357_scale_9.jpg)
##### SPI(12)
![Comparison between spi raw data (SPI-12)](outputs/station_3357_scale_12.jpg)
##### SPI(24)
![Comparison between spi raw data (SPI-24)](outputs/station_3357_scale_24.jpg)

### Comparison between the densities of accumulated precipitation for basin S-3216

##### SPI(3)
![Comparison of accumulated precipitation (SPI-3)](outputs/compareAccumDens_station_3357_scale_3.jpg)
##### SPI(6)
![Comparison of accumulated precipitation (SPI-6)](outputs/compareAccumDens_station_3357_scale_6.jpg)
##### SPI(9)
![Comparison of accumulated precipitation (SPI-9)](outputs/compareAccumDens_station_3357_scale_9.jpg)
##### SPI(12)
![Comparison of accumulated precipitation (SPI-12)](outputs/compareAccumDens_station_3357_scale_12.jpg)
##### SPI(24)
![Comparison of accumulated precipitation (SPI-24)](outputs/compareAccumDens_station_3357_scale_24.jpg)


### Relationship between the bias introduced to the training data and the change in the mean monthly precipitation

##### SPI(3)
![bias vs change in precipitation (SPI-3)](outputs/biasTrendGrowth_scale3.jpg)
##### SPI(6)
![bias vs change in precipitation (SPI-6)](outputs/biasTrendGrowth_scale6.jpg)
##### SPI(9)
![bias vs change in precipitation (SPI-9)](outputs/biasTrendGrowth_scale9.jpg)
##### SPI(12)
![bias vs change in precipitation (SPI-12)](outputs/biasTrendGrowth_scale12.jpg)
##### SPI(24)
![bias vs change in precipitation (SPI-24)](outputs/biasTrendGrowth_scale24.jpg)

</div>
