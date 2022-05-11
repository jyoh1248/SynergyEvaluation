# SynergyEvaluation
Interaction effects evaluation for in vitro combinations of two compounds, R package + Simulation code

## Index
  - [Overview](#Overview) 
  - [Description](#Description) 
  - [Authors](#Authors)
  - [Reference](#Reference)

## Overview
- Project name : Interacction effects evaluation (Sanofi)
- Project period : 2020/04 - 2020/11 ((Sanofi))
- Package updated : 2020/11, SynergyEvaluation_1.0.5.2020.tar.gz


With the aim to develop better treatment regimens with increased efficacy and/or reduced toxicity, combination therapies have been adopted as the standard of care for various diseases and especially in oncology. A combination of multiple effective drugs encumbers the necessity of methods to evaluate synergistic interaction between compounds for over a century. However, the idea of synergy is not restricted to only the biomedical field as it has been developed across many different disciplines. The first step in the combination therapy development is to evaluate the interaction effects with in-vitro combination experiments.


## Description

> Interaction effects evaluation for in vitro combinations of two compouds : comparision of methods in variable relative potency cases (two compouns with nonparallel dose-effect curves)


The internship's mission includes the overview of the current statistical methods to analyze compounds combinations with in-vitro experiments in the case where the relative potency is not constant (maximum effects and/or slopes not shared between the dose-effect curves of single compounds). The methods for explicit detection of synergy will be discussed as well as the proposition of alternative methods like Tallarida model and Hand model. The implementation of methods, including development of a R package called \texttt{SynergyEvaluation}, is conducted with simulations of experimental data. Finally results are presented visually, in order to facilitate the understanding and the comparison between methods.

![simulation](/source/simulation.png)

### Notations

Explanation of important notations
![notion](/source/notion0.png)
![notion](/source/notion.png)

### Model(1) : Loewe model

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

## Reference    
- Berenbaum, M. C. What is synergy? *Pharmacological reviews* 41,(1989).

- Christian T., David J., et al., Quantifying Drug Combination Synergy along Potency and Efficacy Axes,*Cell Systems*, 8, 2 (2019).

- de Souza GE., Bueno RV., de Souza JO. et al. Antiplasmodial profile of selected compounds from Malaria Box: in vitro evaluation, speed of action and drug combination studies, *Malaria Journal* 18, 447 (2019).
    
- Foucquier J., Guedji M., Analysis of drug combinations: current methodological landscape,*Pharmacology Research & Perspectives*, (2015).
     
- Geary N. Understanding synergy. American journal of physiology, *Endocrinology and metabolism*, 304(3), E237–E253 (2013).

- Grabovsky Y., Tallarida RJ., Isobolographic analysis for combinations of a full and partial agonist: Curved isoboles, *The journal of pharmacology and experimental therapeutics* 310, 981–986 (2004).
    
- Greco WR., Bravo G., Parsons JC., The search for synergy: A critical review from a response surface perspective. *Pharmacological reviews* 47, 331–385 (1995).
    
- Habron C., A flexible unified approach to the analysis of pre-clinical combination studies,*Statistics in Medicine*, (2020).
     
- Huang RY., Pei L., et al., Isobologram Analysis: A Comprehensive Review of Methodology and Current Research,
     \textit{Frontiers in pharmacology}, 10, 1222 (2019). 
     
     \bibitem{gen.response}
     Kong M., Lee JJ., A generalized response surface model with varying relative potency for assessing drug interaction, \textit{Biometrics} 62(4):986-95 (2016).
     
     \bibitem{synergyS} 
     Lederer S, Dijkstra TMH., Heskes T. Additive dose response models: defining synergy, 
     \textit{Frontiers in Pharmacology}, 10, 1384 (2019).
     
     \bibitem{Combitherapy} 
     Lee J., Kong M., Ayers GD., Lotan R., Interaction Index and Different Methods for Determining Drug Interaction in Combination Therapy, 
     \textit{Journal of Biopharmaceutical Statistics}, 17:3, 461-480 (2007).
     
     \bibitem{sanofi} 
     Mangin M., Windenberger F., In-vitro combination studies - guides for statisticians, statistical report, Sanofi R\&D.
     
     \bibitem{Nonblending} 
     Peterson JJ., Novick SJ. Nonlinear blending: a useful general concept for the assessment of combination drug synergy,  \textit{Journal of receptor and signal transduction research}, 27(2-3), 125–146 (2007).
     
     \bibitem{term1}
     Roell KR., Reif DM., Motsinger-Reif AA.,  An Introduction to Terminology and Methodology of Chemical Synergy-Perspectives from Across Disciplines, 
     \textit{Frontiers in pharmacology}, 8, 158 (2017).
     
    \bibitem{drc} 
     Ritz C., Baty F., Streibig JC., Gerhard D. Dose-Response Analysis Using R, 
     \textit{PLoS ONE}, 10(12): e0146021 (2015).
     
     \bibitem{Hand} 
     Sinzger M., Vanhoefer J., Loos C. et al. Comparison of null models for combination drug therapy reveals Hand model as biochemically most plausible, 
     \textit{Scientific Reports} 9, 3002 (2019).

     \bibitem{design}
     Straetemans R. et al., Design and analysis of drug combination experiments, 
     \textit{Biometrical journal} vol. 47,3 (2005).
     
    \bibitem{T2001}
     Tallarida RJ., Drug Synergism: It's detection and applications, \textit{The journal of pharmacology and experimental therapeutics} (2001).
     
     \bibitem{T2005}
     Tallarida RJ., Comments on "Isobolographic Analysis for Combinations of a Full and Partial Agonist: Curved Isoboles", \textit{The journal of pharmacology and experimental therapeutics}, (2005).
     
     \bibitem{T2006} 
     Tallarida RJ., An overview of drug combination analysis with isobolograms, \textit{The journal of pharmacology and experimental therapeutics}, 319, 1–7(2006).
     
     \bibitem{T2007} 
     Tallarida RJ., Interactions between drugs and occupied receptors, \textit{The journal of pharmacology and experimental therapeutics}, 113: 197–209 (2007).

     \bibitem{T2012}
     Tallarida RJ., Revisiting the isobole and related quantitative methods for assessing drug synergism, \textit{Perspective in Pharmacology}, (2012).
     
     \bibitem{iso}
     Tallarida RJ, Drug combinations: Tests and Analysis with Isoboles, 
     \textit{Current Protocols in Pharmacology}, (2017).
     
     \bibitem{Saariselka} 
     Tang, J., Wennerberg K., Aittokallio T., What is synergy? The Saariselkä agreement revisited, 
     \textit{Frontiers in Pharmacology}, 6, 181 (2015).
     
     \bibitem{integral} 
     Tao T., An Introduction to Measure Theory, (2011).
