<div align="center">
  <img src="misc/logoo.png" alt="EcoShower Logo" width="200"/>

  # EcoShower

  [![R](https://img.shields.io/badge/R-%23E67E22.svg?&logo=R&logoColor=white)](https://www.r-project.org/)
  [![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54) 
  [![PyTorch](https://img.shields.io/badge/PyTorch-%23EE4C2C.svg?&logo=PyTorch&logoColor=white)](https://pytorch.org/)
  [![License: GPL-3.0](https://img.shields.io/badge/License-GPL%203.0-blue.svg)](https://opensource.org/licenses/GPL-3.0)

  *Non-intrusive water management using multi-modal sensors and deep learning*

  [Key Features](#key-features) •
  [Project Overview](#project-overview) •
  [Data and Analysis](#data-and-analysis) •
  [Citation](#citation)
</div>

---

## Key Features  

🚿 **Accurate Shower Duration Estimation**
- Utilizes humidity, temperature, sound average, and sound peak sensor data.
- Non-intrusive multi-modal sensor approach.

🤖 **Advanced Deep Learning Models**
- Includes Bidirectional LSTM and Gated Transformer Networks (GTNs).
- Supports multivariate time series classification.

🌍 **Environmentally Conscious Design**
- Aimed at sustainable water management.
- Analyzes sensor contributions to optimize resource usage.

📊 **Data Insights and Usability**
- Visualizes predictions and sensor contributions.
- Evaluates sensor configurations for practical applications.

## Project Overview

EcoShower tackles the challenge of accurately estimating shower duration using non-intrusive, multi-modal sensor data. This project explores the use of humidity, temperature, and sound sensors to enable efficient water management and promote sustainability. By leveraging state-of-the-art machine learning models, including Bidirectional Long Short-Term Memory (LSTM) networks and Gated Transformer Networks (GTNs) [citation: [GTN Paper](https://arxiv.org/abs/2103.14438)], this work demonstrates the potential of advanced analytics in non-intrusive time series classification.

### Highlights:
- **Sensor Modalities**: Humidity, temperature, sound average, and sound peak.
- **Advanced Analytics**: Use of Bidirectional LSTMs for sequence analysis and GTNs for multivariate time series classification.
- **Real-World Applications**: Designed for both residential and commercial water conservation initiatives.

This repository contains the scripts, datasets, and models used in the analysis.

The preprint of this work is currently under review and is available at [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4880452).

## Data and Analysis

### Dataset
The dataset comprises multi-modal sensor readings:
- **Humidity**: Measured in relative humidity (%)
- **Temperature**: Measured in degrees Celsius
- **Sound**: Average and peak sound intensity in decibels (dB SPL)
- **Ground Truth**: Start and stop events recorded using LoRaWAN-enabled buttons

### Analysis Workflow
1. **Data Cleaning and Preprocessing**: Sensor data synchronization, noise reduction, and transformation into time series format.
2. **Modeling Approaches**:
   - Bidirectional LSTM: Captures sequential dependencies in shower events.
   - Gated Transformer Network (GTN): Explores both temporal and channel-wise correlations for multivariate time series classification.
3. **Evaluation**:
   - Accuracy metrics for classification tasks.
   - Sensor importance analysis to identify the optimal configuration for practical use.

### Results
Our analysis revealed:
- **Best Performance**: Achieved using all sensors, with over 97% accuracy for shower duration estimation.
- **Key Sensors**: Humidity and temperature provide sufficient accuracy for most use cases, highlighting the potential for minimal sensor configurations.
- **Real-World Feasibility**: Models were effective in predicting shower durations while respecting privacy concerns associated with sound data.

## Citation

If you use EcoShower in your research, please cite:

```bibtex
@article{sablica4880452ecoshower,
  title={Ecoshower: Estimating Shower Duration Using Non-Intrusive Multi-Modal Sensor Data Via LSTM and Gated Transformer Models},
  author={Sablica, Lukas and Gr{\"u}n, Bettina and Layeghy, Siamak and Dolnicar, Sara and Portmann, Marius},
  journal={Available at SSRN 4880452}
}
```




