
# Market Stress Detection with Logistic Regression ML

## Overview

This project builds a **machine learning based early warning system** to identify periods of elevated market risk.  
The model estimates the **probability of market stress** using recent volatility, cumulative losses, and drawdowns.

---


## Use Case
This system can be used as:
- A daily **market risk dashboard input**
- An **early warning indicator** for risk committees
- A prioritization tool when monitoring multiple markets or portfolios

## Data
- Public S&P 500 daily closing prices (2008–2024)

---

## Features
- 10-day and 20-day rolling volatility  
- 20-day cumulative log return  
- 20-day drawdown from recent peak  

These indicators capture uncertainty, sustained losses, and stress severity.

---

## Stress Definition (target)
A day is labeled as **stress** if:
- 20-day return < −5%, **or**
- 20-day volatility exceeds its historical 95th percentile

This rule defines the target; ML learns to recognize these conditions probabilistically.

---

## Model
- Logistic Regression with class balancing
- Time-based train/test split
- Output: probability of market stress

---

## Evaluation
Performance is assessed using **precision and recall**
Different probability thresholds reflect different **risk appetites**, trading off early detection against false alerts.
