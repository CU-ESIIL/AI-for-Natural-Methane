# Data Processing Documentation

ACCESS proposal

Applying Knowledge-Guided Machine Learning to Global Biogeochemical Cycles
 
Youmi Oh, University of Colorado Boulder (PI)
Licheng Liu, University of Minnesota (Co-PI)

Research or Education Objectives
The main research objective of this project is to apply knowledge-guided machine learning (KGML) to global biogeochemical cycles. The synergy of process-based and machine-learning modeling, which takes advantage of the strengths of each approach, has gained attention recently in earth science fields and is called ‘Hybrid modeling’ or ‘KGML’ (ElGhawi et al. 2022; Daw et al. 2017) (Fig. 1). The growing field of KGML (Karpatne, Kannan, and Kumar 2022; Willard et al. 2023) provides a promising modeling method that combines the advantages of process-based models, machine-learning models, and multi-source datasets. KGML has successfully modeled certain earth systems in which dynamic processes are physically constrained by established governing equations, such as in hydrology and atmospheric sciences (Irrgang et al. 2021; Willard et al. 2023; Read et al. 2019; Kraft et al. 2022; Beucler et al. 2021). 
Despite the early success of the KGML approach, research in combining process-based and machine-learning models in global biogeochemical cycles is still at a nascent stage. This is because the processes in terrestrial biogeochemical ecosystems are mathematically complicated and highly non-linear, and the direct measurements of these processes are often expensive and limited. There are a few inspiring studies that use KGML to estimate soil biogeochemical processes. For example, Co-PI Dr. Liu adapted KGML into agricultural processes with knowledge-guided initialization and modified machine-learning architecture designs and showed a significant improvement in simulating soil N2O and net ecosystem exchange (NEE) fluxes (Liu et al. 2022). In this project, we propose to further develop a novel KGML framework by synthesizing the process-based and machine-learning models, and multi-source direct and indirect observations to accurately quantify the spatial and temporal variability in global biogeochemical cycles.

Description of Resource Needs
This proposed data synthesis will heavily rely on developing, pretraining, and training the KGML framework using big datasets, which demand substantial high-performance computational resources, especially GPU computing resources. We will use ACCESS resources to meet the needs of such requirements. Around 1000~3000 GPU-hours (based on Nvidia A-100 GPU) and 10TB storage will be requested. The development environment will be primarily on Python, with the Pytorch library installed. Any Python libraries pre-required by Pytorch (details in https://pytorch.org/) will also be installed.

References:
Beucler, Tom, Michael Pritchard, Stephan Rasp, Jordan Ott, Pierre Baldi, and Pierre Gentine. 2021. “Enforcing Analytic Constraints in Neural Networks Emulating Physical Systems.” Physical Review Letters 126 (9): 098302.
Daw, Arka, Anuj Karpatne, William Watkins, Jordan Read, and Vipin Kumar. 2017. “Physics-Guided Neural Networks (PGNN): An Application in Lake Temperature Modeling.” arXiv [cs.LG]. arXiv. http://arxiv.org/abs/1710.11431.
ElGhawi, Reda, Basil Kraft, Christian Reimers, Markus Reichstein, Marco Körner, Pierre Gentine, and Alexander J. Winkler. 2022. “Hybrid Modeling of Evapotranspiration: Inferring Stomatal and Aerodynamic Resistances Using Combined Physics-Based and Machine Learning.” Earth and Space Science Open Archive. https://doi.org/10.1002/essoar.10512258.1.
Irrgang, Christopher, Niklas Boers, Maike Sonnewald, Elizabeth A. Barnes, Christopher Kadow, Joanna Staneva, and Jan Saynisch-Wagner. 2021. “Towards Neural Earth System Modelling by Integrating Artificial Intelligence in Earth System Science.” Nature Machine Intelligence 3 (8): 667–74.
Karpatne, Anuj, Ramakrishnan Kannan, and Vipin Kumar. 2022. Knowledge Guided Machine Learning: Accelerating Discovery Using Scientific Knowledge and Data. CRC Press.
Kraft, Basil, Martin Jung, Marco Körner, Sujan Koirala, and Markus Reichstein. 2022. “Towards Hybrid Modeling of the Global Hydrological Cycle.” Hydrology and Earth System Sciences 26 (6): 1579–1614.
Liu, Licheng, Shaoming Xu, Jinyun Tang, Kaiyu Guan, Timothy J. Griffis, Matthew D. Erickson, Alexander L. Frie, et al. 2022. “KGML-Ag: A Modeling Framework of Knowledge-Guided Machine Learning to Simulate Agroecosystems: A Case Study of Estimating N 2 O Emission Using Data from Mesocosm Experiments.” Geoscientific Model Development 15 (7): 2839–58.
Read, Jordan S., Xiaowei Jia, Jared Willard, Alison P. Appling, Jacob A. Zwart, Samantha K. Oliver, Anuj Karpatne, et al. 2019. “Process‐guided Deep Learning Predictions of Lake Water Temperature.” Water Resources Research 55 (11): 9173–90.
Willard, Jared, Xiaowei Jia, Shaoming Xu, Michael Steinbach, and Vipin Kumar. 2023. “Integrating Scientific Knowledge with Machine Learning for Engineering and Environmental Systems.” ACM Computing Surveys. https://doi.org/10.1145/3514228.


## Overview
Brief description of the data processing objectives and scope. Reminder to adhere to data ownership and usage guidelines.

## Data Sources
List and describe data sources used, including links to cloud-optimized sources. Highlight permissions and compliance with data ownership guidelines.

## CyVerse Discovery Environment
Instructions for setting up and using the CyVerse Discovery Environment for data processing. Tips for cloud-based data access and processing.

## Data Processing Steps

### Using GDAL VSI
Guidance on using GDAL VSI (Virtual System Interface) for data access and processing. Example commands or scripts:
```bash
gdal_translate /vsicurl/http://example.com/data.tif output.tif
```

## Cloud-Optimized Data
Advantages of using cloud-optimized data formats and processing data without downloading. Instructions for such processes.

## Data Storage

Information on storing processed data, with guidelines for choosing between the repository and CyVerse Data Store.

## Best Practices

Recommendations for efficient and responsible data processing in the cloud. Tips to ensure data integrity and reproducibility.

## Challenges and Troubleshooting

Common challenges in data processing and potential solutions. Resources for troubleshooting in the CyVerse Discovery Environment.

## Conclusions

Summary of the data processing phase and its outcomes. Reflect on the methods used.

## References

Citations of tools, data sources, and other references used in the data processing phase.
