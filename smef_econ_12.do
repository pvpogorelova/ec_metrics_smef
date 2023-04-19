clear 
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem12.log

* Задание 1. Бутстрап

set seed 1234 // для воспроизводимости результатов
set obs 1000 // число наблюдений

* Функция invnorm(uniform()) генерирует стандартную нормальную СВ

* Сгенерируем объясняющую переменную
gen x = invnorm(uniform())

* Сгенерируем остатки модели в виде нормально распределенной СВ с нулевым мат. ожиданием и дисперсией равной 3
gen e = sqrt(3)*invnorm(uniform())

* Сгенерируем зависимую переменную
gen y = 0.4*x + e

* Оценим по сгенерированным данным линейную модель регрессии без константы
reg y x, noc

* Сгенерируем B = 1000 бутстрапированных выборок для линейной регрессии и рассчитаем бутстрапированные стандартные ошибки,
* доверительные интервалы и p-value
bootstrap, reps(1000): reg y x

* Доверительные интервалы для коэффициентов, основанные на бутстрапированных процентилях
estat bootstrap, percentile

* Бутстрапируем t-статистику для коэффициента наклона и рассчитаем критические значения на 5% уровне значимости
reg y x
scalar b = _b[x]
bootstrap t=((_b[x]-b)/_se[x]), reps(999): reg y x, level(95)
estat bootstrap, percentile

* Соответствующие симметричные критические значения на уровне значимости 5% вычисляются следующим образом
reg y x
scalar b = _b[x]
bootstrap t = abs((_b[x]-b)/_se[x]), reps(999): reg y x, level(90)
estat bootstrap, percentile

* Можно создать программу для вычисления t-статистики
program tstat, rclass
reg y x
return scalar t = (_b[x]-b)/_se[x]
end

reg y x
scalar b = _b[x]
bootstrap t=r(t), reps(1000): tstat
estat bootstrap, percentile

reg y x
scalar b = _b[x]
bootstrap t=r(t), reps(999) saving(bs_t3): tstat
use bs_t3, replace
centile t, centile(2.5, 97.5)
gen t_abs = abs(t)
centile t_abs, centile(95)

reg y x
scalar b = 0.6
tstat
return list

* Задание 2. Непараметрическое оценивание
use schooling.dta, clear

twoway kdensity lwage76 || hist lwage76 // ядерная плотность с использованием ядра Епанечникова
kdensity lwage76, kernel(gaussian) normal // ядерная плотность с использованием гауссовского ядра

scatter lwage76 exp76
scatter lwage76 age76

reg lwage76 exp76
reg lwage76 age76


* Локально полиномиальная регрессия Kernel-weighted local polynomial smoothing
* Rule-of-thumb используется по умолчанию для выбора bwidth()
lpoly lwage76 exp76, gen(g s) ci

twoway scatter lwage76 exp76 || ///
lpoly lwage76 exp76, degree(2) kernel(epan2) || ///
lpoly lwage76 exp76, degree(1) kernel(epan2) bwidth(1) || ///
lpoly lwage76 exp76, degree(1) kernel(epan2) bwidth(5)


* Задание 3. Полупараметрическая регрессия
* Пример на сгенерированных данных
clear
set obs 1000 //  число наблюдений

drawnorm e // сгенерируем остатки

gen z = (uniform()-0.5)*30 

gen x1 = z+invnorm(uniform())
gen x2 = z+invnorm(uniform())
gen x3 = z+invnorm(uniform())

generate y = x1+x2+x3+e

replace y = (10*sin(abs(z)))*(z<_pi)+y

* Оценим, используя полупараметрический подход
net search semipar // Robinson's double residual estimator (1998)
semipar y x*, nonpar(z) generate(fit_rob) partial(res)

net search plreg // Yatechew's difference estimator (1998)
plreg y x*, nlf(z) gen(fit_yat)


* Рассмотрим пример на реальных данных
use schooling.dta, clear

scatter lwage76 exp76

* Обычная линейная модель регрессии
reg lwage76 ed76 exp76 exp762 black smsa76 south76

* Теперь предположим, что переменная exp76 включена в модель нелинейно, а остальные факторы входят линейно
* Таким образом, получаем полупараметрическую модель
plreg lwage76 ed76 black smsa76 south76, nlf(exp76) gen(lwage_yat) order(1) // order(#) is an optional argument to specify the differencing order
semipar lwage76 ed76 black smsa76 south76, nonpar(exp76) ci gen(lwage_rob) robust test(2) // H0: Параматерическая и непараметрическая оценка совпадают

* Рассчитаем передельные эффекты по непараметрической части
bysort ed76: gen ok = (_n == 1)
dydx lwage_rob ed76 if ok == 1, gen(lwage_prim)
bysort ed76: replace lwage_prim = lwage_prim[1]
twoway (line lwage_prim ed76)
