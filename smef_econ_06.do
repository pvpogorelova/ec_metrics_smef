clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23
log using sem6.log

* Logit-, probit-модели

clear

* Логит-модель
use http://www.stata.com/data/jwooldridge/eacsap/mroz, clear
describe
summarize

reg inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценим линейную модель вероятности
predict prob_ols // прогноз по линейной модели вероятности
sum prob_ols // описательные статистики прогнозной вероятности "успеха"

logit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // оценивание logit-модели с помощью ММП (логистические шансы). Коэффициент при переменной показывает, во сколько раз изменится логарифм отношения P("удачи")/P("неудачи")
logit, or //  odds ratio (отношение шансов P("удачи")/P("неудачи")=exp(xi'b)). При увеличении xj на 1 единицу шансы события {yi=1} вырастают в exp(bj) раз.
* или альтернативная команда
logistic inlf educ exper expersq age kidslt6 kidsge6 nwifeinc // odds ratio
predict pl0, pr // прогноз вероятности "успеха"
sum pl0 // описательные статистики прогнозной вероятности "успеха"

logit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc, vce(robust) // оценивание logit-модели с помощью ММП (логистические шансы)
logit, or // odds ratio (отношение шансов P("удачи")/P("неудачи")=exp(xi'b)). При увеличении xj на 1 единицу шансы события {yi=1} вырастают в exp(bj) раз.

predict pl1, pr // прогнозирование вероятности "успеха"
sum pl1 // описательные статистики прогнозной вероятности "успеха"

test kidslt6 = kidsge6 = 0 // тестирование гипотезы о незначимости переменных, характеризующих состав детей в семье

margins, dydx(*) // усредненные предельные эффекты 
margins, dydx(*) atmeans // предельные эффекты (marginal effects) для "среднего" наблюдения. ПЭ для переменной xj показывает, на сколько изменится вероятность "успеха" при увеличении xj на 1 единицу
margins, at(educ = (10(2)20)) // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) vsquish // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) atmeans // вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года при средних значениях остальных переменных

net search fitstat // загрузка fitstat
fitstat // различные показатели качества моделей из разных классов, в том числе и бинарных

lsens, genprob(prob_cutoff) gensens(sens) genspec(spec) title("График зависимости чувствительности и специфичности") // график зависимости Se и Sp от cut-off
gen difference = sens-spec
levelsof sens if abs(difference)<0.01, local(yval)
levelsof prob_cutoff if sens ==`yval', local(xval)
lsens, yline(`yval') xline(`xval') scheme(s1color) ylab(0 0.2 `yval' 0.8 1) xlab(0 0.2 `xval' 0.8 1)
lroc // построение ROC-кривой и расчет AUC
estat clas, cutoff(0.55) // таблица классификации (сопряженности y^ и y)
estat gof // H0: модель адекватна (Хи2-критерий Пирсона)


scalar cut_off = 0.55
gen inlf_f = 0
replace inlf_f = 1 if pl1 > cut_off // создание зависимой переменной, полученной в результате оценивания модели и соответствующей оптимальному параметру cut-off

* Пробит-модель
probit inlf educ exper expersq age kidslt6 kidsge6 nwifeinc
probit, or
predict probit_pr, pr

margins, dydx(*) atmeans
margins, dydx(*)

fitstat

* Multinomial logistic regression
use "/Users/polinapogorelova/Downloads/PENSION.DTA", clear
* Данные в “PENSION.dta” представляют собой подмножество данных, использованных Papke (1998) при оценке влияния предоставления отдельным лицам возможности самостоятельно 
* выбирать распределение средств в их пенсионных планах. Первоначально Papke кодирует ответы "в основном облигации", "смешанные" и "в основном акции" как 0, 50 и 100, 
*и использует модель линейной регрессии, оцененную OLS. Выбор бинарной объясняющей переменной choose равен единице, если у человека есть выбор как инвестирует средства 
* его или ее пенсионный фонд.
describe

gen pctstck1 = 1
replace pctstck1 = 2 if pctstck == 50
replace pctstck1 = 3 if pctstck == 100

mlogit pctstck1 choice age educ i.female i.married wealth89 prftshr, base(1)
predict p_1 p_2 p_3, pr // прогноз вероятности принадлежности индивидуума к каждой категории
mlogit, rrr // relative-risk ratios (показывает, чему равно отношение вероятностей принадлежности к разным группам при увеличении объясняющей переменной на 1 единицу)
test [2]1.female = [3]1.female

margins i.female, pr(out(1))

margins, dydx(female) predict(out(1)) // предельный эффект, рассчитанный для переменной black
margins, dydx(female) predict(out(2))
margins, dydx(female) predict(out(3))

margins i.female, atmeans pr(out(1)) // прогноз вероятности выбрать школу (y=1) в зависимости от значения факторной переменной black при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(oblig)
graph export oblig, as(png) width(600) height(450) replace
margins i.female, atmeans pr(out(2))
marginsplot, name(Mixed)
margins i.female, atmeans pr(out(3))
marginsplot, name(Shares)

graph combine oblig Mixed Shares, ycommon

margins i.female, pr(out(3)) // средняя вероятность того, что y=3 на каждом уровне факторной переменной black

margins, at(educ = (8(2)18)) predict(outcome(1)) vsquish // средняя вероятность того, что y=1 на каждом уровне количественной переменной educ (от 8 до 18 с шагом равным 2 годам)
margins, at(educ = (8(2)18)) predict(outcome(2)) vsquish
margins, at(educ = (8(2)18)) predict(outcome(3)) vsquish

fitstat // проверка качества модели

* Ordered logit regression
use https://stats.idre.ucla.edu/stat/data/ologit.dta, clear // данные о поступлении в аспирантуру
tab apply
tab apply, nolabel
tab apply public
sum gpa

ologit apply i.pared i.public gpa // оценим порядковый логит. Коэффициенты показывают во сколько раз увеличится логарифм отношения шансов при изменении объясняющей переменной на 1 единицу
ologit apply i.pared i.public gpa, or // получим оценки изменения отношения шансов (odds ratio) перейти с одного уровня на более высокий
listcoef, help

margins, at(pared = (0/1)) predict(outcome(0)) atmeans // прогнозируемая вероятность попасть в низшую категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(1)) atmeans // прогнозируемая вероятность попасть в среднюю категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(2)) atmeans // прогнозируемая вероятность попасть в высшую категорию в зависимости от значения категориальной переменной "pared"

forvalues i = 0/2 {
  margins, at(gpa = 3.5 pared = 1 public = 1) predict(outcome(`i'))
} // прогноз вероятности попасть в каждую категорию для индивидуума, имеющего GPA равный 3.5, обучавшегося в частной школе и у которого хотя бы один из родителей обучался в аспирантуре

net search omodel
omodel logit apply pared public gpa // альтернативная команда для оценивания упорядоченных моделей логит и пробит, содержащая также реультаты теста на пропорциональность шансов (test for the equivalence of the estimates for cut levels)
predict prob_unlikely prob_somewhatlikely prob_verylikely, pr // прогнозирование вероятностей попасть в каждую категорию
clear

log close
