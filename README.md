# rym - R пакет для работы с API Яндекс.Метрики <a href='https://selesnow.github.io/ryandexdirect/'><img src='https://raw.githubusercontent.com/selesnow/rym/master/inst/logo/rym.png' align="right" height="139" /></a>
===

CRAN
====

[![Rdoc](http://www.rdocumentation.org/badges/version/rym)](https://www.rdocumentation.org/packages/rym)

Официальная документация к пакету rym
=====================================

Официальную русскоязычную документацию можно найти по этой [ссылке](https://selesnow.github.io/rym/)

## Краткое описание
================

`rym` является R интерфейсом для работы с API Яндекс Метрики, его функции позволяют вам взаимодействовать со следующими API:

1.  [API Управления](https://yandex.ru/dev/metrika/doc/api2/management/intro-docpage) - позволяет получить таблицы с такими объектами как достуные счёт��ики Яндекс.Метрики, список настроенных целей, фильтров и сегментов, а так же список пользователей у которых есть доступ к счётчику.
2.  [API Отчётов](https://yandex.ru/dev/metrika/doc/api2/api_v1/intro-docpage) - позволяет получать информацию о статистике посещений сайта и другие данные, не используя интерфейс Яндекс.Метрики.
3.  [API совместимый с Core API Google Analytics (v3)](https://yandex.ru/dev/metrika/doc/api2/ga/intro-docpage) - позволяет запрашивать статистические данные используя при этом название полей такие же как и при работе с Core Reporting API v3.
4.  [Logs API](https://yandex.ru/dev/metrika/doc/api2/logs/intro-docpage) - позволяет получить сырые, несгруппированные данные о посещении вашего сайта из Яндекс.Метрики.

## Установка
---------

**Важно:** Версия на CRAN устарела. Для установки актуальной версии с исправленной аутентификацией используйте метод установки с GitHub.

CRAN (старая версия): `install.packages('rym')`

GitHub (рекомендуется): `devtools::install_github("danzerzine/rym")`

## Исправление аутентификации
============================

Процесс аутентификации в этом пакете был нарушен, поскольку он зависел от внешнего веб-сервиса для получения токена, который перестал функционировать. Это было исправлено следующим образом:

1.  **Ручное получение кода**: Вместо неработающей веб-страницы теперь вам будет предложено скопировать URL-адрес, на который вас перенаправит Яндекс после авторизации, прямо в консоль R. Скрипт автоматически извлечет из него необходимый код авторизации.
2.  **Исправлен запрос токена**: Исправлена ошибка в коде, из-за которой запрос на получение токена завершался неудачей после ввода кода авторизации.

Теперь процесс аутентификации снова полностью функционален.

---

## Authentication Fix
====================

The authentication process in this package was broken because it relied on an external web helper to fetch the authentication token, which is no longer online. This has been fixed:

1.  **Manual Code Extraction**: Instead of the broken helper page, you will now be prompted to paste the entire redirect URL from your browser into the R console after authenticating with Yandex. The script will extract the authorization code from it automatically.
2.  **Token Request Fix**: A bug that caused the token request to fail after entering the authorization code has been corrected.

The authentication flow is now fully functional again.

## Виньетки
========

Помимо официальной документации у пакета есть 5 виньеток, вводная, и отдельно виньетка под каждый API, открыть их можно с помощью следующих команд:

-   Введение в пакет `rym`: `vignette('intro-to-rym', package = 'rym')`
-   API Управления: `vignette('rym-management-api', package = 'rym')`
-   API Отчётов: `vignette('rym-reporting-api', package = 'rym')`
-   API совместимый с Core API Google Analytics v3: `vignette('rym-ga-api', package = 'rym')`
-   Logs API: `vignette('rym-logs-api', package = 'rym')`

Пример кода
-----------

```r
# auth
rym_auth(login = "your_yandex_login")


# ManagementAPI
# get counters list
my_counters <- rym_get_counters()

# get goals list
my_goals <- rym_get_goals(counter = my_counters$id[1])

# пget filter list
my_filter <- rym_get_filters(counter = my_counters$id[1])

# get segment list
my_segments <- rym_get_segments(counter = my_counters$id[1])

# get counter list
users <- rym_users_grants(counter = my_counters$id[1])

# Reporting API
reporting.api.stat <- rym_get_data(counters   = my_counters$id[1],
                                   date.from  = "2022-08-01",
                                   date.to    = "yesterday",
                                   dimensions = "ym:s:date,ym:s:lastTrafficSource",
                                   metrics    = "ym:s:visits,ym:s:pageviews,ym:s:users",
                                   sort       = "-ym:s:date",
                                   lang = "en")

# Logs API
logs.api.stat      <- rym_get_logs(counter    = my_counters$id[1],
                                   date.from  = "2022-08-01",
                                   date.to    = "2022-08-05",
                                   fields     = "ym:s:date,
                                                 ym:s:lastTrafficSource,
                                                 ym:s:referer",
                                   source     = "visits")

# API compatible with Core API Google Analytics v3
ga.api.stat        <- rym_get_ga(counter    = paste0("ga:", my_counters$id[1]),
                                 dimensions = "ga:date,ga:source",
                                 metrics    = "ga:sessions,ga:users",
                                 start.date = "2022-08-01",
                                 end.date   = "2022-08-05",
                                 sort       = "-ga:date")
```

## Статьи:

- [Как работать с API Яндекс.Метрики с помощью языка R](https://alexeyseleznev.wordpress.com/2018/10/08/%D0%BA%D0%B0%D0%BA-%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%B0%D1%82%D1%8C-%D1%81-api-%D1%8F%D0%BD%D0%B4%D0%B5%D0%BA%D1%81-%D0%BC%D0%B5%D1%82%D1%80%D0%B8%D0%BA%D0%B8-%D1%81-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89/), *Алексей Селезнёв*
- [Как использовать Rscript в качестве источника данных в Microsoft Power BI на примере Яндекс.Метрики](https://www.mediaguru.ru/blog/kak-ispolzovat-rscript-v-kachestve-istochnika-dannyh-v-microsoft-power-bi-na-primere-yandeks-metriki/), *Павел Мрыкин*
- [Построение поведенческих воронок на языке R, на основе данных полученных из Logs API Яндекс.Метрики](https://habr.com/ru/post/462279/), *Алексей Селезнёв*
- [Обзор R пакетов для интернет маркетинга, часть 1](https://habr.com/ru/post/425425/), *Алексей Селезнёв*
- [Насколько безопасно использовать R пакеты для работы с API рекламных систем](https://habr.com/ru/post/430888/), *Алексей Селезнёв*
- [Как массово удалить в интернет-магазине страницы товаров, которые не приносят трафик](https://netpeak.net/ru/blog/kak-massovo-udalit-v-internet-magazine-stranitsy-tovarov-kotoryye-ne-prinosyat-trafik/), *Богдан Неряхин*
- [Как загружать данные о расходах, офлайн-конверсиях и звонках в Яндекс Метрику](https://alexeyseleznev.wordpress.com/2021/09/01/%d0%ba%d0%b0%d0%ba-%d0%b7%d0%b0%d0%b3%d1%80%d1%83%d0%b6%d0%b0%d1%82%d1%8c-%d0%b4%d0%b0%d0%bd%d0%bd%d1%8b%d0%b5-%d0%be-%d1%80%d0%b0%d1%81%d1%85%d0%be%d0%b4%d0%b0%d1%85-%d0%be%d1%84%d0%bb%d0%b0%d0%b9/), *Алексей Селезнёв*

## Видео уроки:

-   [Как автоматизировать работу с данными Яндекс.Метрики. С помощью языка R](https://www.youtube.com/watch?v=sCp2D6068es)

Автор: Алексей Селезнёв (Head of Analytics Dept. at Netpeak)
Forked and fixed by danzerzine.