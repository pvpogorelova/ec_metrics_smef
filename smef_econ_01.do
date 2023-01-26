clear

set more off // отключить полный вывод результатов работы команд
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23 // директория, в которой хранятся файлы
log using seminar1.log, text replace // начало записи результатов do-файла в seminar1.log


* Множественная регрессия
* Импорт данных
import excel "/Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_22_23", sheet("data") firstrow

gen price_sq = price/totsp // сгенерируем новую зависимую переменную "Стоимость 1 кв.м."
gen kittot = kitsp/livesp // сгенерируем новую объясняющую переменную "Доля кухни"
gen kittot2 = kittot^2

reg price_sq livesp kittot kittot2 metrdist dist
reg price_sq livesp kittot kittot2 metrdist dist, vce(robust) // оценивание модели линейной регрессии с помощью МНК + робастные стандартные ошибки в форме Уайта
eststo model1 // сохраняет копию активных результатов оценки модели регрессии для последующего сведения в таблицу
outreg2 using myreg.doc, replace ctitle(Model 1) bdec(2) sdec(3)  // экспорт результатов оценивания в виде таблицы в файл с форматом .doc (.xls)

help ereturn
ereturn list // вывод на экран результатов оценивания, непредставленных в основном блоке (имена и значения макросов и скаляров, а также имена и размеры матриц, хранящиеся в e()
matrix b = e(b) // матрица коэффициентов
matrix list b // вывод матрицы коэффициентов на экран
matrix cov = e(V) // сохраняем ковариационную матрицу коэффициентов
matrix list cov // вывод ковариационной матрицы на экран
estat vce // вывод на экран ковариационной матрицы оценок коэффициентов модели
 
predict resid_price, r // сохраняем остатки модели в переменную resid_price
hist resid_price, norm // гистограмма распределения остатков
twoway (hist resid_price) (kdensity resid_price)
qnorm resid_price
sktest resid_price // тест Харке-Бера на нормальность (для остатков модели)
swilk resid_price // тест Шапиро-Уилка на нормальность (4<=n<=2000)


predict price_sq_hat // генерирует прогнозные значения
rvfplot, yline(0) // график зависимости остатков от модельных значений (явные признаки гетероскедастичности)
twoway scatter price_sq_hat price_sq // график зависимости прогнозных значений цена квартиры за 1 кв.м. в зависимости от истинных значений переменной


* Тестирование гипотез о равенстве коэффициентов (или их линейной комбинации) некоторому значению
test metrdist = -0.05 // проверка гипотезы о равенстве коэффициента при переменной metrdist значению -0.05
test metrdist = dist // проверка гипотезы о равенстве коэффициентов при переменных metrdist и dist
test metrdist = dist = -0.05 // проверка гипотезы о равенстве коэффициентов при переменны metrdist и dist значению -0.05

reg price_sq totsp kittot kittot2 metrdist dist, beta // регрессия для стандартизированных показателей


* Фиктивные переменные
* i.namevar - способ задания дамми-переменной

gen metrdistwalk = metrdist*walk // создание перекрестной фиктивной переменной

reg price_sq livesp kittot kittot2 metrdist dist i.brick i.walk, vce(robust)

* Построим диаграммы рассеяния "стоимость 1 кв.м. – жилая площадь" в зависимости от кирпичности дома
twoway (scatter price_sq livesp if brick == 0, msymbol(S)) ///
  (scatter price_sq livesp if brick == 1, msymbol(Oh)), ///
  legend(label(1 non brick) label(2 brick))

twoway (scatter price_sq livesp) (lfit price_sq livesp),  by(brick, total)


* Тест Чоу о равенстве коэффициентов модели
* H0: коэффициенты при переменных в моделях, оцененных на разных группах наблюдений, равны
* H1: модели для двух групп наблюдений отличаются хотя бы одним коэффициентом

reg price_sq livesp kittot kittot2 metrdist dist i.brick, vce(robust) // оценивание модели на всей выборке
scalar r_all = e(rss) // сохранение суммы квадратов остатков модели
scalar n_all = e(N) // сохранение общего числа наблюдений

reg price_sq livesp kittot kittot2 metrdist dist i.brick if walk == 1, vce(robust) // оценивание модели для квартир в пешей доступности до метро
scalar r_walk = e(rss) // сохранение суммы квадратов остатков модели, оцененной для квартир, расположенных в пешей доступности от метро
scalar n_walk = e(N) // сохранение общего числа наблюдений
scalar k = e(df_m) + 1 // сохранение числа степеней свободы


reg price_sq livesp kittot kittot2 metrdist dist i.brick if walk == 0, vce(robust) // оценивание модели для квартир, не находящихся в пешей доступности от метро
scalar r_nowalk = e(rss) // сохранение суммы квадратов остатков модели, оцененной для квартир, не расположенных в пешей доступности от метро
scalar n_nowalk = e(N)
scalar ddf = n_nowalk + n_walk - k*2 // расчет степеней свободы для знаменателя


scalar r_tot = r_walk + r_nowalk // сохранение суммы квадратов остатков моделей, оцененных по разным подвыборкам
scalar f_obs = ((r_all-r_tot)/(k))/(r_tot/ddf) // расчет тестовой статистики
scalar pval = Ftail(k,ddf,f_obs) // определение p-value

scalar list ddf f_obs pval // вывод результатов на экран


* Тест Чоу для проверки прогнозной силы модели

reg price_sq livesp kittot kittot2 metrdist dist i.brick in 1/1500, vce(robust) // оценивание модели на ~ 75% выборке
scalar r_p = e(rss) // сохранение суммы квадратов остатков модели
scalar n_p = e(N) // сохранение в память общего числа наблюдений

scalar p = n_all - n_p // сохранение числа степеней свободы числителя
scalar ddf2 = n_p - k // расчет степеней свободы для знаменателя
scalar f_obs2 = ((r_all-r_p)/(p))/(r_p/ddf2) // расчет тестовой статистики
scalar pval2 = Ftail(k,ddf2,f_obs2) // определение p-value

scalar list p ddf2 f_obs2 pval2 // вывод результатов на экран


* Мультиколлинеарность: признаки и последствия
clear
matrix M = (2, 6, 8) // вектор математических ожиданий
matrix list M
matrix C = (10, 7, 5 \ 7, 6, 4 \ 5, 4, 2.73) // ковариационная матрица
matrix list C

* Для начала рассмотрим пример на маленькой выборке размерности 50 наблюдений
set seed 50
drawnorm x1 x2 x3, means(M) cov(C) n(50)
drawnorm eps

gen y = -5 + 3*x1 - 8*x2 + 2*x3 + eps
sum y x1 x2 x3
pwcorr x1 x2 x3, star(0.05)
reg y x1 x2 x3

clear
matrix M = (2, 6, 8)
matrix C = (10, 7, 5 \ 7, 6, 4 \ 5, 4, 2.73)

* Теперь увеличим размер выборки до 1000 наблюдений
set seed 50
set matsize 1000 // увеличение допустимого размера матрицы (по умолчанию 400)
drawnorm x1 x2 x3, means(M) cov(C) n(1000)
drawnorm eps

gen y = -5 + 3*x1 - 8*x2 + 2*x3 + eps
sum y x1 x2 x3
pwcorr x1 x2 x3, star(0.05)
reg y x1 x2 x3

* Пропущенные существенные переменные

clear
matrix M = (2, 6, 8)
matrix C = (10, 7, 5 \ 7, 6, 4 \ 5, 4, 2.73)

set seed 50
set matsize 1000 // увеличение допустимого размера матрицы (по умолчанию 400)
drawnorm x1 x2 x3, means(M) cov(C) n(1000)
scatter x1 x2
scatter x1 x3
scatter x2 x3
drawnorm eps
hist eps, norm
twoway kdensity eps

gen y = -5 + 3*x1 - 8*x2 + 2*x3 + eps
sum y x1 x2 x3
pwcorr x1 x2 x3, star(0.05)
reg y x1 x2 x3
reg y x1 x2


log close // заканчиваем запись в файл
