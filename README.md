# Analysing-the-impact-of-Exchange-Rate-fluctuations-on-major-Stock-Market-Indices
## Introduction:
This project explores the realtionship between **foreign exchange rate fluctuations** and **global stock market indices** in the post covid era. This is examining the impact of **FX fluctuations** on major market indices and identifying volatility and correlation with them. 


## Questions:
1. How do exchange rate fluctuations affect stock market indices?\
2. How do the volatility patterns and correlations between exchange rates and stock indices looks like?\
3. Under different economic regimes, how do these relationships differ?


## Motivation:
Global market volatility post Covid pandemic.\
Major geopolitical events like conflicts between countries.\
The relationship is still unclear and unexplored.\
Potential future implications especially in the Modern Trade war era.


## Methodology & Models:
I have used the different econometrics models for this analyzing such as ADF, ARMA, ACF & PACF, GARCH, DCC-GARCH, VAR, Markov-Switching Models.\

Data analysis implemented using R packages (tidyr, rmgarch, vars, dplyr, readxl,forecast, rugarch, tsDyn, tseries, MSwM)

## Data Description:
### Source: (Yahoo Finance,  01/11/2019- 31/10/2024)

### Stock market indices:
Germany: DAX \
UK: FTSE 100 \
India: NIFTY 50 \
Japan: Nikkei 225 \
US: S&P 100 \
China: Shanghai Composite Index\
Singapore: SGX NIKKEI 225

### Foreign exchange rates :
 CNY/USD, EUR/USD, GBP/USD , INR/USD , JPY/USD , SGD/USD


## Repository Structure

```
├── Main R code.Rmd      # Main R Markdown file for the full analysis
├── Main presentation file.pptx     # Rendered power point presentation (output and results presented of R Markdown)
└── README.md               # This file
```
## Findings/Summary:
Currency-Market Linkages: INR-NIFTY 50 and SGD-SGX \
Some pairs (e.g., CNY-Shanghai Composite ) exhibit weak interdependence.\
Effects of shocks are generally temporary and stabilize over time.\
Regional events and conditions drive time varying correlations.\
Most currency-stock pairs exhibit regime shifts, while some, like Pound/FTSE 100 or EUR-DAX, show greater stability.


## Key Takeaways:
Empirical Evidence is Mixed.\
Direction of Causality and Volatility.\
Modelling Challenges.\
Emerging Markets vs. Developed Markets.


#### Thanks you so much for reading this!
#### Have a good time ahead! 


## Contact
**Tanishq Gupta**
MSc Quantitative Finance\
E: [gu.tanishq@gmail.com](mailto:gu.tanishq@gmail.com) \
LinkedIn: linkedin.com/in/tanishq-gupta 
