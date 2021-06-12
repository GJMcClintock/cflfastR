
# **cfbfastR** <a href='http://saiemgilani.github.io/cfbfastR'><img src='man/figures/logo.png' align="right" height="150" /></a>


The goal of [**`cflfastR`**](https://gjmcclintock.github.io/cflfastR/) is
to provide the community with an R package for working with CFL data. It
is an R API wrapper around <https://api.cfl.ca/>. It will eventually offer
the majority of the functionality offered by cfbfastR and nflfastR.

## **Installation**

You can install the released version of
[**`cfbfastR`**](https://github.com/gjmcclintock/cflfastR/) from
[GitHub](https://github.com/gjmcclintock/cflfastR/) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("gjmcclintock/cflfastR")
```

``` r
# if you would prefer devtools installation
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "gjmcclintock/cflfastR")
```

## **Breaking Changes**

<details>

<summary>View more version news</summary>

## **cfbfastR v0.1.0**
  
- Initial fork providing game info, boxscores for both teams and players, 
  play by play data, and team records/standings.
- It is recommended to save a local copy of files, or load from the cflfastR_data repo. 
  To avoid API limit errors, sleep functions are folded into each function.

#### **Function Naming Convention Change**

 
#### **CFL API Keys**

The [CollegeFootballData API](https://api.cfl.ca/)
requires an API key, here’s a quick run-down:

  - To get an API key, follow the directions here: [CFL
    API Key Request.](https://api.cfl.ca/key-request)

  - Using the key: You can save the key for consistent usage by adding
    `CFL_API_KEY=XXXX-YOUR-API-KEY-HERE-XXXXX` to your .Renviron file
    (easily accessed via
    [**`usethis::edit_r_environ()`**](https://usethis.r-lib.org/reference/edit.html)).
    Run
    [**`usethis::edit_r_environ()`**](https://usethis.r-lib.org/reference/edit.html),
    a new script will pop open named `.Renviron`, **THEN** paste the
    following in the new script that pops up (with**out** quotations)

<!-- end list -->

``` r
CFL_API_KEY = XXXX-YOUR-API-KEY-HERE-XXXXX
```

Save the script and restart your RStudio session, by clicking `Session`
(in between `Plots` and `Build`) and click `Restart R` (there also
exists the shortcut `Ctrl + Shift + F10` to restart your session). If
set correctly, from then on you should be able to use any of the `cfbd_`
functions without any other changes.

  - For less consistent usage: At the beginning of every session or
    within an R environment, save your API key as the environment
    variable `CFL_API_KEY` (with quotations) using a command like the
    following.

<!-- end list -->

``` r
Sys.setenv(CFL_API_KEY = "XXXX-YOUR-API-KEY-HERE-XXXXX")
```

</details>

<br>

# **Our Authors**

  - [Garrett McClintock](https://twitter.com/GJMcClintock)  
    <a href="https://twitter.com/GJMcClintock" target="blank"><img src="https://img.shields.io/twitter/follow/GJMcClintock?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@GJMcClintock" /></a>
    <a href="https://github.com/GJMcClintock" target="blank"><img src="https://img.shields.io/github/followers/GJMcClintock?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

# **Special Thanks**

  - [Andrew Dyck] (https://github.com/andrewjdyck)
  - [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>
