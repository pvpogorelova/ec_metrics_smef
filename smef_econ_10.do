clear
set more off
cd /Users/polinapogorelova/Desktop/vega
log using sem10.log

* Динамические модели
use dahlberg.dta, clear

* Оценим обычную модель регрессии без учета панельной структуры
reg expend l.revenue l.grants l.expend
xtset id year

* Arellano & Bond (GMM) one-step estimator
xtabond expend l.revenue l.grants, lags(1) noconstant

* Тестирование автокорреляции
estat abond // тест на автокорреляцию остатков 1-го и 2-го порядков
* Тест на валидность инструментов (тест Саргана)
estat sargan // H0: все инструменты экзогенны

xtabond expend l.revenue l.grants, lags(1) noconstant vce(robust) // Arellano–Bond robust VCE estimator
xtabond expend l.revenue l.grants, lags(1) vce(robust)

* Arellano & Bond (GMM) two-step estimator
xtabond expend l.revenue l.grants, lags(1) noconstant twostep vce(robust) // twostep
// рекомендуется использовать vce(robust), которая дает состоятельные и асимптотически эффективные оценки при гетероскедастичности - Windmeijer bias-corrected (WC) robust VCE

* Тестирование автокорреляции
estat abond // тест на автокорреляцию остатков 1-го и 2-го порядков
* Тест на валидность инструментов (тест Саргана)
estat sargan // H0: все инструменты экзогенны

clear
