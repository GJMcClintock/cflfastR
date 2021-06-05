#' @title
#' **CFL API Key Registration**
#' @description Save your API Key as a system environment variable `CFL_API_KEY`
#' @details To get access to an API key, follow the instructions at [https://api.cfl.ca/key-request](https://api.cfl.ca/key-request "Key Registration")\cr
#' \cr
#' **Using the key:** \cr
#' You can save the key for consistent usage by adding `CFL_API_KEY=XXXX-YOUR-API-KEY-HERE-XXXXX` to your .Renviron file (easily accessed via [**`usethis::edit_r_environ()`**](https://usethis.r-lib.org/reference/edit.html)). \cr
#' Run [**`usethis::edit_r_environ()`**](https://usethis.r-lib.org/reference/edit.html),
#' a new script will pop open named `.Renviron`, **THEN** \cr
#' paste the following in the new script that pops up (with**out** quotations)
#' ```r
#' CFL_API_KEY = XXXX-YOUR-API-KEY-HERE-XXXXX
#' ```
#' Save the script and restart your RStudio session, by clicking `Session` (in between `Plots` and `Build`) and click `Restart R` \cr
#' (there also exists the shortcut `Ctrl + Shift + F10` to restart your session).
#'
#' If set correctly, from then on you should be able to use any of the `cfl_` functions without any other changes.
#'
#' **For less consistent usage:** \cr
#' At the beginning of every session or within an R environment,
#' save your API key as the environment variable `CFL_API_KEY` (**with** quotations)
#' using a command like the following.
#' ```r
#' Sys.setenv(CFL_API_KEY = "XXX-YOUR-API-KEY-HERE-XXXXX")
#' ```
#' @name register_cfl
NULL
#' @rdname register_cfl
#' @export
cfl_key <- function() {
  key <- Sys.getenv("CFL_API_KEY")

  if (key == "") {
    return(NA_character_)
  } else {
    return(key)
  }
}

#' @rdname register_cfl
#' @export
has_cfl_key <- function() !is.na(cfl_key())
