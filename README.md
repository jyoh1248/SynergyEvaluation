# SynergyEvaluation
Interaction effects evaluation for in vitro combinations of two compounds, R package + Simulation code

## Index
  - [Overview](#Overview) 
  - [Description](#Description) 
  - [Authors](#Authors)

## Overview
- Project name : Interacction effects evaluation (Sanofi)
- Project period : 2020/04 - 2020/11 ((Sanofi))
- Package updated : 2020/11, SynergyEvaluation_1.0.5.2020.tar.gz


With the aim to develop better treatment regimens with increased efficacy and/or reduced toxicity, combination therapies have been adopted as the standard of care for various diseases and especially in oncology. A combination of multiple effective drugs encumbers the necessity of methods to evaluate synergistic interaction between compounds for over a century. However, the idea of synergy is not restricted to only the biomedical field as it has been developed across many different disciplines. The first step in the combination therapy development is to evaluate the interaction effects with in-vitro combination experiments.


## Description

### Interaction effects evaluation for in vitro combinations of two compouds : comparision of methods in variable relative potency cases (two compouns with nonparallel dose-effect curves)


The internship's mission includes the overview of the current statistical methods to analyze compounds combinations with in-vitro experiments in the case where the relative potency is not constant (maximum effects and/or slopes not shared between the dose-effect curves of single compounds). The methods for explicit detection of synergy will be discussed as well as the proposition of alternative methods like Tallarida model and Hand model. The implementation of methods, including development of a R package called \texttt{SynergyEvaluation}, is conducted with simulations of experimental data. Finally results are presented visually, in order to facilitate the understanding and the comparison between methods.

![simulation](/source/simulation.png)

### Notations

Explanation of important notations
![notion](/source/notion0.png)
![notion](/source/notion.png)

### Model (1) : Loewe model

![loewe1](/source/loewe1.png)
![loewe2](/source/loewe2.png)

### Model (2) : Tallarida model

![tallarida1](/source/tallarida1.png)
![tallarida2](/source/tallarida2.png)


### Model (3) : Hand model


![hand1](/source/hand1.png)
![hand2](/source/hand2.png)
![hand3](/source/hand3.png)
![hand4](/source/hand4.png)
![hand5](/source/hand5.png)


## Authors
  - [OH](https://github.com/jyoh1248) - **Jiyoung OH** - <jyoh1248@gmail.com>
  