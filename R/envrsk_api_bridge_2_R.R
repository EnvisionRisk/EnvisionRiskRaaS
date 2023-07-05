#roxygen2::roxygenise()
#******************************************************************************
#*
#* EnvisionRisk Market Risk-as-a-Service API - R-Interface, May 2023
#*
#******************************************************************************
#* EnvisionRisk Market Risk-as-a-Service API is a cloud-based technology solution
#* that provides risk management capabilities through Application Programming
#* Interfaces (APIs). These APIs allow users to access advanced risk analytics,
#* modeling tools, and financial data, facilitating efficient and comprehensive
#* risk assessments.
#*
#* This technology enables financial institutions, asset managers, and other
#* stakeholders to quantify and manage market risk, a key component of financial
#* risk that involves potential losses due to changes in market prices. Market
#* Risk-as-a-Service API can provide real-time, granular risk metrics such as
#* Value at Risk (VaR), Expected Shortfall (ES), and other stress testing measures.
#*
#* Value at Risk (VaR) and Expected Shortfall (ES) are two key metrics used in
#* this practice. VaR estimates the potential loss that could occur in a portfolio
#* over a specific time horizon at a certain confidence level. It's widely used
#* due to its simplicity and intuitive interpretation, but it's criticized for
#* not providing information about the potential losses beyond the VaR threshold,
#* especially during extreme market events.
#*
#* That's where Expected Shortfall comes in. ES, also known as Conditional VaR
#* (CVaR), is a measure that quantifies the expected loss on those occasions
#* when a specific VaR limit is exceeded. By considering the average of all
#* losses worse than the VaR, it provides a more comprehensive picture of tail
#* risk.
#*
#* Together, VaR and ES provide valuable insights into a portfolio's market risk
#* exposure. They guide risk managers in setting risk limits, making strategic
#* asset allocation decisions, and developing hedging strategies to control
#* potential losses. These metrics are essential in the quantitative risk management
#* process, informing decision-making under uncertainty and helping to ensure the
#* financial stability of the firm.
#*
#* The benefits of this service for market risk management are substantial:
#*  - Scalability: As a cloud-based service, it can easily scale to handle large
#*    data volumes and complex analytics, accommodating growing business needs.
#*  - Cost Efficiency: It eliminates the need for heavy upfront investment in
#*    infrastructure and risk modeling software, making sophisticated risk
#*    management capabilities accessible to a broader range of organizations.
#*  - Flexibility: With API-based integration, firms can embed risk analytics
#*    into their existing systems and workflows, promoting seamless and flexible
#*    operations.
#*  - Enhanced Decision-Making: The advanced analytics and comprehensive risk
#*    measures can inform better investment and risk management decisions,
#*    ultimately driving performance and profitability.
#*
#******************************************************************************
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
.datatable.aware = TRUE
options(scipen=999)

#******************************************************************************
#*
#* Wrapping relevant EnvisionRisk Market Risk-as-a-Service API calls into R-functions
#*
#******************************************************************************
#*
#* The functionality wraps the EnvisionRisk market Risk-as-a-Service API into R
#* style functions. The functions takes familiar R data object as input, takes
#* care of communicating with the cloud server and transform the JSON output
#* from the API into R data structures.
#*
.onLoad = function (libname, pkgname) {
  assign('api_url', 'https://api.envisionrisk.com/', envir = topenv())
  assign('api_path', 'v1/themis/', envir = topenv())
}

get_api_url <- function(end_point){
  api_url <- paste0(api_url,
                    api_path,
                    end_point)
  return(api_url)
}

get_access_token <- function(){
  auth_flow_response <- envrsk_auth_renew_access_token()

  if(auth_flow_response[["status-code"]] == 200){
    return(Sys.getenv("ACCESS_TOKEN"))
  }
  stop(auth_flow_response)
}

#' Function to make a POST request to an API
#'
#' This function sends a POST request to the specified API URL.
#' It requires an access token, a URL, a body for the POST request,
#' and a list of query parameters. It adds the access token to the
#' headers of the request and sends the POST request using the 'httr' package.
#'
#' @param access_token A string containing the access token.
#' @param url A string containing the URL of the API endpoint.
#' @param body A list containing the body of the POST request.
#' @param params A list containing the query parameters for the POST request.
#'
#' @return A list containing the status code and the content of the API response.
#' @keywords internal
envrsk_post <- function(access_token,
                        url,
                        body,
                        params){

  # Query parameters
  .query <- c(params)

  #' headers
  .headers <- httr::add_headers('ACCESS-TOKEN' = access_token)

  # Body
  .body <- jsonlite::toJSON(body)

  #' post call to the endpoint
  res <- httr::POST(
    url    = url,
    query  = .query,
    config = .headers,
    body   = .body,
    httr::accept_json()
  )

  return(list("status_code" = httr::status_code(res),
              "content"     = httr::content(res)))
}

#' Function to make a GET request to an API
#'
#' This function sends a GET request to the specified API URL.
#' It requires an access token, a URL, and a list of query parameters.
#' It adds the access token to the headers of the request and sends
#' the GET request using the 'httr' package.
#'
#' @param access_token A string containing the access token.
#' @param url A string containing the URL of the API endpoint.
#' @param params A list containing the query parameters for the GET request.
#'
#' @return A list containing the status code and the content of the API response.
#' @keywords internal
envrsk_get <- function(access_token,
                       url,
                       params){

  # Query parameters
  .query <- c(params)

  #' headers
  .headers <- httr::add_headers('ACCESS-TOKEN' = access_token)


  #' post call to the endpoint
  res <- httr::GET(
    url    = url,
    query  = .query,
    config = .headers,
    httr::accept_json()
  )

  return(list("status_code" = httr::status_code(res),
              "content"     = httr::content(res)))
}

#******************************************************************************
#### Auth ####
#******************************************************************************
#' Function to get access token for API authentication
#'
#' This function obtains an access token needed for API authentication.
#' It requires a user ID and password, and sends a GET request to the
#' specified URL to retrieve the access token. If the request is successful,
#' it sets the expiry time for the access token to 24 hours from the current time.
#'
#' @param usr_id A string containing the user ID.
#' @param usr_pwd A string containing the user password.
#'
#' @return A list containing the status code, the access token and its expiry time.
#'         In case of error, it returns the error message.
#'
#' @examples
#' \dontrun{
#' token <- envrsk_auth_get_access_token(usr_id = "your_user_id",
#'                                       usr_pwd = "your_password")
#' }
#' @keywords internal
envrsk_auth_get_access_token <- function(usr_id, usr_pwd){
  # Query parameters
  .query <- list("usr_id"  = usr_id,
                 "usr_pwd" = usr_pwd)

  if(is.null(usr_id) | is.na(usr_id) | usr_id == ""){
    return(list("status-code" = 400,
                "message" = "Missing required parameter: usr_id"))
  }

  if(is.null(usr_pwd) | is.na(usr_pwd) | usr_pwd == ""){
    return(list("status-code" = 400,
                "message" = "Missing required parameter: usr_pwd"))
  }

  access_token_expiry <- Sys.time() + 24*60*60
  get_access_token <- httr::GET(
    url    = "https://api.envisionrisk.com/auth/get-access-token",
    query  = .query
  )

  if(httr::status_code(get_access_token) != 200){
    return(list("status-code" = httr::status_code(get_access_token),
                "message"     = httr::content(get_access_token)))
  }
  access_token <- httr::content(get_access_token)
  return(list("status-code"         = 200,
              "access-token"        = access_token,
              "access-token-expiry" = access_token_expiry))

}

#' Function to renew access token for API authentication
#'
#' This function renews the access token needed for API authentication.
#' It optionally forces a token renewal. If user ID and password are
#' available in the system environment variables, it uses them to get
#' a new access token. If the current access token exists and has not
#' expired, it continues to use it unless force_renew is TRUE.
#'
#' @param force_renew A boolean value indicating whether to force the renewal
#'                    of the access token. Default is FALSE.
#'
#' @return A message indicating the validity of the access token or any error message.
#'
#' @examples
#' \dontrun{
#' envrsk_auth_renew_access_token(force_renew = TRUE)
#' }
#' @keywords internal
envrsk_auth_renew_access_token <- function(force_renew = FALSE){
  renew_flow <- function(){
    if(Sys.getenv("USR_ID") != "" & Sys.getenv("USR_PWD") != ""){
      access_token <- envrsk_auth_get_access_token(
        Sys.getenv("USR_ID"),
        Sys.getenv("USR_PWD"))
      if(access_token[["status-code"]] == 200){
        Sys.setenv("ACCESS_TOKEN"        = access_token[["access-token"]],
                   "ACCESS_TOKEN_EXPIRY" = as.character(as.POSIXct(
                     access_token[["access-token-expiry"]],
                     format = "%Y-%m-%d %h:%m:%s")))
        return(list("status-code" = 200,
                    "message" = "access-token has been aquired"))
      } else {
        Sys.setenv("USR_ID" = "")
        Sys.setenv("USR_PWD" = "")
        return(access_token)
      }
    } else {
      envrsk_auth_set_access_token()
    }
  }

  if(force_renew | Sys.getenv("ACCESS_TOKEN") == ""){
    Sys.setenv("USR_ID" = "")
    Sys.setenv("USR_PWD" = "")
    renew_flow()
  } else {
    cond <- try(Sys.getenv("ACCESS_TOKEN_EXPIRY") < Sys.time(), TRUE)
    if(is.logical(cond) && cond){
      renew_flow()
    } else {
      return(list("status-code" = 200,
                  "message" = paste0("access-token is valid until: ", Sys.getenv("ACCESS_TOKEN_EXPIRY"))))
    }
  }
}


#' Function to set access token for API authentication
#'
#' This function sets the environment variables for user ID and password,
#' and retrieves an access token for API authentication. It uses the user's
#' input for the email and password as credentials for authentication.
#' The access token retrieved is stored globally in the 'my_access_token' variable.
#'
#' @return No return value. This function prints a message if the access token is
#' successfully retrieved.
#'
#' @examples
#' \dontrun{
#' envrsk_auth_set_access_token()
#' }
#' @keywords internal
envrsk_auth_set_access_token <- function(){
  # Provide credentials - email and password. In case you have not yet received
  # your personal credentials, contact EnvisionRisk at info@envisionrisk.com
  Sys.setenv("USR_ID"  = getPass::getPass(msg = "Please provide email: ",
                                          noblank = TRUE, forcemask = FALSE))
  Sys.setenv("USR_PWD" = getPass::getPass(msg = "Please provide password: ",
                                          noblank = TRUE, forcemask = FALSE))

  # AUTHENTICATIO WITH THE RISK SERVER
  # Retrieve the access-token from the Auth-server.
  access_token <- envrsk_auth_get_access_token(
    Sys.getenv("USR_ID"),
    Sys.getenv("USR_PWD"))
  if(access_token[["status-code"]] == 200){
    Sys.setenv("ACCESS_TOKEN"        = access_token[["access-token"]],
               "ACCESS_TOKEN_EXPIRY" = as.character(as.POSIXct(
                  access_token[["access-token-expiry"]],
                  format = "%Y-%m-%d %h:%m:%s")))
    #message("access-token has been aquired")
    return(list("status-code" = 200,
                "message" = "access-token has been aquired"))
  } else {
    return(list("status-code" = 400,
                "message" = access_token))
  }
}

#' Function to log out from the API
#'
#' This function logs out from the API by removing the environment variables used.
#'
#' @return No return value. This function clears certain variables and environment variables.
#' @export
#'
#' @examples
#' \dontrun{
#' envrsk_auth_log_out()
#' }
envrsk_auth_log_out <- function(){
  Sys.setenv("USR_ID"  = "",
             "USR_PWD" = "",
             "ACCESS_TOKEN"        = "",
             "ACCESS_TOKEN_EXPIRY" = "")
}

#******************************************************************************
#### Portfolio ####
#******************************************************************************
#' Function for estimating portfolio risk - Value-at-Risk (VaR) and Expected
#' Shortfall (ES)
#'
#' This function uses the 'portfolio-risk-regular' API endpoint to estimate
#' portfolio risk. It requires an access token and a date, along with a
#' list of positions. It optionally takes in a base currency, a horizon,
#' a significance level, a volatility ID, a report depth, and a flag
#' to simplify the output.
#'
#' @param date A date for which the portfolio risk should be estimated.
#' @param positions A list of positions in the portfolio.
#' @param base_cur An optional base currency for the portfolio. Default is NULL.
#' @param horizon An optional time horizon for the risk estimate. Default is NULL.
#' @param signif_level An optional significance level for the risk measure.
#' Default is NULL.
#' @param volatility_id An optional volatility ID. Default is NULL.
#' @param report_depth An optional depth of the report. Default is NULL.
#' @param simplify A flag indicating whether to simplify the output. Default is FALSE.
#'
#' Value at Risk (VaR) is a statistical measure that estimates the maximum potential
#' loss over a specified time horizon at a given confidence level. Expected
#' Shortfall (ES), also known as Conditional Value at Risk (CVaR), estimates the
#' average loss in the event that the VaR is exceeded. Both are forward-looking
#' as they use statistical analysis based on historical data and volatility to
#' predict potential future losses. They are essential for market risk management,
#' helping organizations understand their risk exposure and potential financial
#' impact under adverse market conditions.
#'
#' @return A processed portfolio return value.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#' result_1 <- envrsk_portfolio_risk_regular(access_token = "your_token",
#'                                           date         = "2022-12-31",
#'                                           positions    = dt_positions)
#'
#' result_2 <- envrsk_portfolio_risk_regular(access_token  = "your_token",
#'                                           date          = "2022-12-31",
#'                                           positions     = dt_positions,
#'                                           base_cur      = "USD",
#'                                           horizon       = 1,
#'                                           signif_level  = 0.975,
#'                                           volatility_id = "point_in_time",
#'                                           report_depth  = 0,
#'                                           simplify      = TRUE)
#' }
envrsk_portfolio_risk_regular <- function(date,
                                          positions,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL,
                                          report_depth  = NULL,
                                          simplify      = FALSE){
  end_point <- "portfolio-risk-regular"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Function for estimating risk components of a portfolio
#'
#' This function uses the 'portfolio-risk-regular' API endpoint to estimate
#' portfolio risk. It requires an access token and a date, along with a
#' list of positions. It optionally takes in a base currency, a horizon,
#' a significance level, a volatility ID, a report depth, and a flag
#' to simplify the output.
#'
#' @param date A date for which the portfolio risk should be estimated.
#' @param positions A list of positions in the portfolio.
#' @param base_cur An optional base currency for the portfolio. Default is NULL.
#' @param horizon An optional time horizon for the risk estimate. Default is NULL.
#' @param signif_level An optional significance level for the risk measure.
#' Default is NULL.
#' @param volatility_id An optional volatility ID. Default is NULL.
#' @param report_depth An optional depth of the report. Default is NULL.
#' @param simplify A flag indicating whether to simplify the output. Default is FALSE.
#'
#' Value at Risk (VaR) is a statistical measure that estimates the maximum potential
#' loss over a specified time horizon at a given confidence level. Expected
#' Shortfall (ES), also known as Conditional Value at Risk (CVaR), estimates the
#' average loss in the event that the VaR is exceeded. Both are forward-looking
#' as they use statistical analysis based on historical data and volatility to
#' predict potential future losses. They are essential for market risk management,
#' helping organizations understand their risk exposure and potential financial
#' impact under adverse market conditions.
#'
#' Component Value at Risk (ComponentVaR) and Component Expected Shortfall
#' (ComponentES) are measures that break down the total VaR and ES of a portfolio
#' into individual components corresponding to each asset in the portfolio.
#'
#' ComponentVaR for a particular asset represents the contribution of that asset
#' to the portfolio's overall VaR. Similarly, CES quantifies an individual asset's
#' contribution to the portfolio's total expected shortfall.
#'
#' These measures are crucial for market risk management as they provide detailed
#' insights into the risk contribution of each asset in the portfolio. By
#' identifying the assets that contribute most to the portfolio's risk, a risk
#' manager can better optimize the portfolio's risk-return tradeoff. For example,
#' they may decide to reduce the portfolio's exposure to assets with high CVaR or
#' CES, diversify the asset allocation, or use hedging strategies to mitigate risk.
#'
#' @return A processed portfolio return value.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#' result_1 <- envrsk_portfolio_risk_component(date         = "2022-12-31",
#'                                           positions    = dt_positions)
#'
#' result_2 <- envrsk_portfolio_risk_component(date          = "2022-12-31",
#'                                           positions     = dt_positions,
#'                                           base_cur      = "USD",
#'                                           horizon       = 1,
#'                                           signif_level  = 0.975,
#'                                           volatility_id = "point_in_time",
#'                                           report_depth  = 0,
#'                                           simplify      = TRUE)
#' }
envrsk_portfolio_risk_component <- function(date,
                                            positions,
                                            base_cur      = NULL,
                                            horizon       = NULL,
                                            signif_level  = NULL,
                                            volatility_id = NULL,
                                            report_depth  = NULL,
                                            simplify      = FALSE){
  end_point <- "portfolio-risk-component"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Function to calculate the delta vector of a portfolio
#'
#' This function sends a POST request to the 'portfolio-delta-vector' API endpoint
#' to calculate the delta vector of a portfolio on a specified date.
#' It requires an access token, a date, and the positions in the portfolio.
#' It also allows for optional parameters like base currency, horizon,
#' volatility ID, and report depth. It returns the response from the API.
#'
#' @param date A string containing the date for the calculation.
#' @param positions A list containing the positions in the portfolio.
#' @param base_cur An optional string containing the base currency.
#' @param horizon An optional numeric containing the horizon.
#' @param signif_level An optional numeric containing the significant level.
#' @param volatility_id An optional string containing the volatility ID.
#' @param report_depth An optional numeric containing the report depth.
#' @param simplify An optional logical value indicating whether to simplify the
#' return values.
#'
#' @return A list containing the API response.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' delta_vector_1 <- envrsk_portfolio_delta_vector(date         = "2022-12-31",
#'                                                 positions    = dt_positions)
#'
#' delta_vector_2 <- envrsk_portfolio_delta_vector(date          = "2022-12-31",
#'                                                 positions     = dt_positions,
#'                                                 base_cur      = "USD",
#'                                                 horizon       = 1,
#'                                                 volatility_id = "point_in_time",
#'                                                 report_depth  = 0,
#'                                                 simplify      = TRUE)
#' }
envrsk_portfolio_delta_vector <- function(date,
                                          positions,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL,
                                          report_depth  = NULL,
                                          simplify      = FALSE){
  end_point <- "portfolio-delta-vector"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Function to calculate the economic capital of a portfolio
#'
#' This function sends a POST request to the 'portfolio-economic-capital-regular'
#' API endpoint
#' to calculate the economic capital of a portfolio on a specified date.
#' It requires an access token, a date, and the positions in the portfolio.
#' It also allows for optional parameters like base currency, horizon,
#' volatility ID, expected ROE (Return on Equity), and report depth.
#' It returns the response from the API.
#'
#' @param date A string containing the date for the calculation.
#' @param positions A list containing the positions in the portfolio.
#' @param base_cur An optional string containing the base currency.
#' @param horizon An optional numeric containing the horizon.
#' @param signif_level An optional numeric containing the significant level.
#' @param volatility_id An optional string containing the volatility ID.
#' @param expected_roe An optional numeric containing the expected Return on
#' Equity (ROE).
#' @param report_depth An optional numeric containing the report depth.
#' @param simplify An optional logical value indicating whether to simplify the
#' return values.
#'
#' Economic capital in market risk management is the amount of risk capital a
#' company holds to withstand unexpected losses due to market risk. It's essentially
#' a safety net that protects the company's financial health from severe market
#' downturns. It's computed based on statistical measure Expected Shortfall (ES),
#' which estimate potential losses under extreme but plausible scenarios. A higher
#' economic capital indicates a larger buffer against market volatility but also
#' may suggest greater exposure to risk. It's crucial in risk management, capital
#' allocation, and strategic decision-making.

#' @return A list containing the API response.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' economic_capital_1 <- envrsk_portfolio_economic_capital_regular(date         = "2022-12-31",
#'                                                                 positions    = dt_positions)
#'
#' economic_capital_2 <- envrsk_portfolio_economic_capital_regular(date          = "2022-12-31",
#'                                                                 positions     = dt_positions,
#'                                                                 base_cur      = "USD",
#'                                                                 horizon       = 10,
#'                                                                 signif_level  = 0.99,
#'                                                                 volatility_id = "downturn",
#'                                                                 expected_roe  = 0.10,
#'                                                                 report_depth  = 0,
#'                                                                 simplify      = TRUE)
#' }
envrsk_portfolio_economic_capital_regular <- function(date,
                                                      positions,
                                                      base_cur      = NULL,
                                                      horizon       = NULL,
                                                      signif_level  = NULL,
                                                      volatility_id = NULL,
                                                      expected_roe  = NULL,
                                                      report_depth  = NULL,
                                                      simplify      = FALSE){
  end_point <- "portfolio-economic-capital-regular"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Function to calculate the component economic capital of a portfolio
#'
#' This function sends a POST request to the 'portfolio-economic-capital-component'
#' API endpoint
#' to calculate the economic capital of a portfolio on a specified date.
#' It requires an access token, a date, and the positions in the portfolio.
#' It also allows for optional parameters like base currency, horizon,
#' volatility ID, expected ROE (Return on Equity), and report depth.
#' It returns the response from the API.
#'
#' @param date A string containing the date for the calculation.
#' @param positions A list containing the positions in the portfolio.
#' @param base_cur An optional string containing the base currency.
#' @param horizon An optional numeric containing the horizon.
#' @param signif_level An optional numeric containing the significant level.
#' @param volatility_id An optional string containing the volatility ID.
#' @param expected_roe An optional numeric containing the expected Return on Equity (ROE).
#' @param report_depth An optional numeric containing the report depth.
#' @param simplify An optional logical value indicating whether to simplify the return values.
#'
#' Economic capital in market risk management is the amount of risk capital a
#' company holds to withstand unexpected losses due to market risk. It's essentially
#' a safety net that protects the company's financial health from severe market
#' downturns. It's computed based on the statistical measure Expected Shortfall (ES),
#' which estimate potential losses under extreme but plausible scenarios. A higher
#' economic capital indicates a larger buffer against market volatility but also
#' may suggest greater exposure to risk. It's crucial in risk management, capital
#' allocation, and strategic decision-making.
#'
#' @return A list containing the API response.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' economic_capital_1 <- envrsk_portfolio_economic_capital_component(date         = "2022-12-31",
#'                                                                   positions    = dt_positions)
#'
#' economic_capital_2 <- envrsk_portfolio_economic_capital_component(date          = "2022-12-31",
#'                                                                   positions     = dt_positions,
#'                                                                   base_cur      = "USD",
#'                                                                   horizon       = 10,
#'                                                                   signif_level  = 0.99,
#'                                                                   volatility_id = "downturn",
#'                                                                   expected_roe  = 0.10,
#'                                                                   report_depth  = 0,
#'                                                                   simplify      = TRUE)
#' }
envrsk_portfolio_economic_capital_component <- function(date,
                                                        positions,
                                                        base_cur      = NULL,
                                                        horizon       = NULL,
                                                        signif_level  = NULL,
                                                        volatility_id = NULL,
                                                        expected_roe  = NULL,
                                                        report_depth  = NULL,
                                                        simplify      = FALSE){
  end_point <- "portfolio-economic-capital-component"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Compute Portfolio Hypothetical Risk Adjusted Performance (Regular)
#'
#' This function communicates with a given API endpoint to compute and return the
#' hypothetical risk adjusted performance of a given portfolio,
#' considering positions provided.
#'
#' @param date         A string in the format 'yyyy-mm-dd', required for the API
#' call. The date of the portfolio.
#' @param positions    A data frame or matrix. Each row represents a position
#' in the portfolio with necessary details.
#' @param base_cur     A string representing the base currency for the portfolio.
#' Default is NULL.
#' @param horizon      Numeric. The time horizon in days for which the value at
#' risk is calculated. Default is NULL.
#' @param signif_level Numeric. The significance level for the value at risk
#' calculation. Default is NULL.
#' @param volatility_id An optional identifier for a specific volatility model
#' to use in the calculation. Default is NULL.
#' @param expected_roe Expected return on equity, as a decimal. Default is NULL.
#' @param report_depth The depth to which the report should calculate risk. Default
#' is NULL.
#' @param simplify     Logical indicating whether the result should be simplified,
#' if possible. Default is FALSE.
#'
#' @return If simplify = FALSE, a list with the following components is returned:
#'         - "Input": A list containing the input parameters used in the API call.
#'         - "tech_opr": The timestamp of when the API call was made.
#'         - "Output": A data.table with the calculated risk performance.
#'         - "symbols_mapped": A data.table with the symbol mapping.
#'         - "symbols_unmapped": A data.table with the symbols that could not be mapped.
#'         If simplify = TRUE, only the "Output" data.table is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' result <- envrsk_portfolio_hyp_rskadj_perf_regular(date         = "2023-05-20",
#'                                                    positions    = dt_positions,
#'                                                    base_cur     = "USD",
#'                                                    horizon      = 1,
#'                                                    signif_level = 0.95,
#'                                                    expected_roe = 0.1,
#'                                                    report_depth = 3,
#'                                                    simplify = TRUE)
#' }
envrsk_portfolio_hyp_rskadj_perf_regular <- function(date,
                                                     positions,
                                                     base_cur      = NULL,
                                                     horizon       = NULL,
                                                     signif_level  = NULL,
                                                     volatility_id = NULL,
                                                     expected_roe  = NULL,
                                                     report_depth  = NULL,
                                                     simplify      = FALSE){
  end_point <- "portfolio-hyp-rskadj-perf-regular"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Compute Portfolio Hypothetical Risk Adjusted Performance (Component)
#'
#' This function communicates with a given API endpoint to compute and return the
#' hypothetical risk adjusted performance of a given portfolio,
#' considering positions provided.
#'
#' @param date         A string in the format 'yyyy-mm-dd', required for the API
#' call. The date of the portfolio.
#' @param positions    A data frame or matrix. Each row represents a position
#' in the portfolio with necessary details.
#' @param base_cur     A string representing the base currency for the portfolio.
#' Default is NULL.
#' @param horizon      Numeric. The time horizon in days for which the value at
#' risk is calculated. Default is NULL.
#' @param signif_level Numeric. The significance level for the value at risk
#' calculation. Default is NULL.
#' @param volatility_id An optional identifier for a specific volatility model
#' to use in the calculation. Default is NULL.
#' @param expected_roe Expected return on equity, as a decimal. Default is NULL.
#' @param report_depth The depth to which the report should calculate risk. Default
#' is NULL.
#' @param simplify     Logical indicating whether the result should be simplified,
#' if possible. Default is FALSE.
#'
#' @return If simplify = FALSE, a list with the following components is returned:
#'         - "Input": A list containing the input parameters used in the API call.
#'         - "tech_opr": The timestamp of when the API call was made.
#'         - "Output": A data.table with the calculated risk performance.
#'         - "symbols_mapped": A data.table with the symbol mapping.
#'         - "symbols_unmapped": A data.table with the symbols that could not be mapped.
#'         If simplify = TRUE, only the "Output" data.table is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' result <- envrsk_portfolio_hyp_rskadj_perf_component(date         = "2023-05-20",
#'                                                      positions    = dt_positions,
#'                                                      base_cur     = "USD",
#'                                                      horizon      = 1,
#'                                                      signif_level = 0.95,
#'                                                      expected_roe = 0.1,
#'                                                      report_depth = 3,
#'                                                      simplify = TRUE)
#' }
envrsk_portfolio_hyp_rskadj_perf_component <- function(date,
                                                       positions,
                                                       base_cur      = NULL,
                                                       horizon       = NULL,
                                                       signif_level  = NULL,
                                                       volatility_id = NULL,
                                                       expected_roe  = NULL,
                                                       report_depth  = NULL,
                                                       simplify      = FALSE){
  end_point <- "portfolio-hyp-rskadj-perf-component"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "expected_roe"  = expected_roe,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' envrsk_portfolio_hypothetical_performance function
#'
#' This function calculates the hypothetical performance of a portfolio over a given time period. It sends a request to an external API endpoint
#' and processes the return values.
#'
#' @param date A character string representing the date for the performance calculation.
#' @param positions A dataset representing the portfolio positions.
#' @param base_cur Optional. A character string representing the base currency for the calculation. Default is NULL.
#' @param report_depth Optional. A numeric value representing the depth of the report. Default is NULL.
#' @param simplify Optional. A logical value. If TRUE, the result is simplified to a vector or matrix if possible. Default is FALSE.
#'
#' @return This function returns processed portfolio return values based on the response from the API call.
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#' "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' port_hyp_perf_1 <- envrsk_portfolio_hypothetical_performance(date         = Sys.Date(),
#'                                                              positions    = dt_positions)
#'
#' port_hyp_perf_2 <- envrsk_portfolio_hypothetical_performance(date          = "2023-06-30",
#'                                                              positions     = dt_positions,
#'                                                              base_cur      = "USD",
#'                                                              report_depth  = 0,
#'                                                              simplify      = TRUE)
#'}
#' @export
envrsk_portfolio_hypothetical_performance <- function(date,
                                            positions,
                                            base_cur      = NULL,
                                            report_depth  = NULL,
                                            simplify      = FALSE){
  end_point <- "portfolio-hyp-perf"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  out <- process_portfolio_return_values(res_out, simplify)
  return(out)
}

#' Process API Response
#'
#' This function processes the response from the portfolio risk API.
#' It binds rows of the 'Output', 'Positions_Mapped', and 'Positions_UnMapped' lists
#' into data frames using the 'rbindlist' function from the 'data.table' package.
#' If the 'simplify' parameter is TRUE, it returns only the 'Output' data frame;
#' otherwise, it returns the entire response.
#'
#' @param res_out A list containing the API response.
#' @param simplify A logical value indicating whether to simplify the return values.
#'
#' @return A list or a data frame depending on the 'simplify' parameter.
#'         If the API response status code is not 200, it returns the original response.
#'
#' @examples
#' \dontrun{
#' processed_output <- process_portfolio_return_values(res_out = api_response, simplify = TRUE)
#' }
#' @noRd
process_portfolio_return_values <- function(res_out, simplify){
  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(!is.null(out[["Positions_Mapped"]])){
      out[["Positions_Mapped"]] <- data.table::rbindlist(out[["Positions_Mapped"]], fill = TRUE)
    }

    if(!is.null(out[["Positions_UnMapped"]])){
      out[["Positions_UnMapped"]]     <- data.table::rbindlist(out[["Positions_UnMapped"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
}

#******************************************************************************
#### Instrument ####
#******************************************************************************
#' Instrument Search
#'
#' This function calls the 'search-instrument' endpoint of the EnvisionRisk API
#' to search for financial instruments by partial name, symbol, exchange ID, or
#' position type.
#' The search is done based on the current or specified date.
#'
#' @param partial_name A string, a partial name of the financial instrument.
#' @param partial_symbol A string, a partial symbol of the financial instrument.
#' @param partial_exchange_id A string, a partial exchange ID of the financial instrument.
#' @param position_type A string, the type of position to search for.
#' @param valid_at A string, date at which the financial instrument must be valid (YYYY-MM-DD).
#'
#' @return A data frame containing the details of the instruments matching the search criteria
#'         or the original API response if the status code is not 200.
#' @export
#'
#' @examples
#' \dontrun{
#' instruments <- envrsk_instrument_search(partial_name = "AAPL")
#' }
envrsk_instrument_search <- function(partial_name        = NULL,
                                     partial_symbol      = NULL,
                                     partial_exchange_id = NULL,
                                     position_type       = NULL,
                                     valid_at            = NULL){
  end_point <- "search-instrument"
  api_url <- get_api_url(end_point)

  .params <- list("partial_name"        = partial_name,
                  "partial_symbol"      = partial_symbol,
                  "partial_exchange_id" = partial_exchange_id,
                  "position_type"       = position_type,
                  "valid_at"            = valid_at)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = list())

  if(res_out[["status_code"]] == 200){
    out <- data.table::rbindlist(res_out[["content"]][["Output"]], fill = TRUE)
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Performance
#'
#' This function calls the 'instrument-performance' endpoint of the EnvisionRisk API
#' to retrieve performance data for a set of financial instruments.
#' The performance is calculated over a specified period and frequency.
#'
#' @param symbols A list, the symbols of the financial instruments for which
#' performance data is required.
#' @param base_cur A string, the base currency in which the performance is calculated.
#' @param from A string, the start date for the performance period (YYYY-MM-DD).
#' @param to A string, the end date for the performance period (YYYY-MM-DD).
#' @param days An integer, the frequency at which performance data is calculated.
#' @param direction A string, whether the performance is calculated in a leading ('lead')
#' or lagging ('lag') manner.
#' @param overlap A boolean, whether overlapping returns are allowed in the calculation.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output performance data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' performance <- envrsk_instrument_performance(access_token = "your_access_token",
#'                                              symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_performance <- function(symbols,
                                          base_cur  = NULL,
                                          from      = NULL,
                                          to        = NULL,
                                          days      = 1,
                                          direction = "lead",
                                          overlap   = TRUE){
  end_point <- "instrument-performance"
  api_url <- get_api_url(end_point)

  .params <- list("base_cur"  = base_cur,
                  "from"      = from,
                  "to"        = to,
                  "days"      = days,
                  "direction" = direction,
                  "overlap"   = overlap)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Performance
#'
#' This function calls the 'instrument-performance-raw' endpoint of the EnvisionRisk API
#' to retrieve performance data for a set of financial instruments. The endpoint does not
#' apply currency conversion. The performance is calculated over a specified period
#' and frequency.
#'
#' @param symbols A list, the symbols of the financial instruments for which performance
#' data is required.
#' @param from A string, the start date for the performance period (YYYY-MM-DD).
#' @param to A string, the end date for the performance period (YYYY-MM-DD).
#' @param days An integer, the frequency at which performance data is calculated.
#' @param direction A string, whether the performance is calculated in a leading
#' ('lead') or lagging ('lag') manner.
#' @param overlap A boolean, whether overlapping returns are allowed in the calculation.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output performance data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' performance <- envrsk_instrument_performance_raw(symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_performance_raw <- function(symbols,
                                              from      = NULL,
                                              to        = NULL,
                                              days      = 1,
                                              direction = "lead",
                                              overlap   = TRUE){
  end_point <- "instrument-performance-raw"
  api_url <- get_api_url(end_point)

  .params <- list("from"      = from,
                  "to"        = to,
                  "days"      = days,
                  "direction" = direction,
                  "overlap"   = overlap)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Value at Risk
#'
#' This function calls the 'instrument-value-at-risk' endpoint of the EnvisionRisk API
#' to compute the Value at Risk (VaR) for a set of financial instruments.
#' VaR is a statistical measure that quantifies the level of financial risk within
#' a firm or investment portfolio over a specific time frame.
#'
#' @param date A string, the date at which VaR is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which VaR
#' is to be computed.
#' @param base_cur A string, the base currency in which VaR is to be computed.
#' @param horizon An integer, the time horizon over which VaR is computed.
#' @param signif_level A numeric, the significance level for the VaR calculation
#' (between 0 and 1).
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output VaR data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' var <- envrsk_instrument_value_at_risk(date = "2023-06-01",
#'                                        symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_value_at_risk <- function(date,
                                     symbols,
                                     base_cur      = NULL,
                                     horizon       = NULL,
                                     signif_level  = NULL,
                                     volatility_id = NULL){
  end_point <- "instrument-value-at-risk"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Value at Risk
#'
#' This function calls the 'instrument-value-at-risk-raw' endpoint of the EnvisionRisk API
#' to compute the Value at Risk (VaR) for a set of financial instruments.
#' VaR is a statistical measure that quantifies the level of financial risk within a
#' firm or investment portfolio over a specific time frame.
#'
#' @param date A string, the date at which VaR is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which VaR
#' is to be computed.
#' @param horizon An integer, the time horizon over which VaR is computed.
#' @param signif_level A numeric, the significance level for the VaR calculation
#' (between 0 and 1).
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output VaR data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' var <- envrsk_instrument_value_at_risk_raw(access_token = "your_access_token",
#'                                            date = "2023-06-01",
#'                                            symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_value_at_risk_raw <- function(date,
                                         symbols,
                                         horizon       = NULL,
                                         signif_level  = NULL,
                                         volatility_id = NULL){
  end_point <- "instrument-value-at-risk-raw"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Expected Shortfall
#'
#' This function calls the 'instrument-expected-shortfall' endpoint of the EnvisionRisk
#' API
#' to compute the Expected Shortfall (ES), also known as Conditional Value at Risk
#' (CVaR), for a set of financial instruments.
#' ES is a risk measure that quantifies the expected value of loss given that an
#' event beyond the VaR threshold has occurred.
#'
#' @param date A string, the date at which ES is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which ES
#' is to be computed.
#' @param base_cur A string, the base currency in which ES is to be computed.
#' @param horizon An integer, the time horizon over which ES is computed.
#' @param signif_level A numeric, the significance level for the ES calculation
#' (between 0 and 1).
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output ES data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' es <- envrsk_instrument_expected_shortfall(date = "2023-06-01",
#'                                            symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_expected_shortfall <- function(date,
                                          symbols,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL){
  end_point <- "instrument-expected-shortfall"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Expected Shortfall
#'
#' This function calls the 'instrument-expected-shortfall-raw' endpoint of the
#' EnvisionRisk API
#' to compute the Expected Shortfall (ES), also known as Conditional Value at
#' Risk (CVaR), for a set of financial instruments.
#' ES is a risk measure that quantifies the expected value of loss given that
#' an event beyond the VaR threshold has occurred.
#'
#' @param date A string, the date at which ES is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which ES
#' is to be computed.
#' @param horizon An integer, the time horizon over which ES is computed.
#' @param signif_level A numeric, the significance level for the ES calculation
#' (between 0 and 1).
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output ES data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' es <- envrsk_instrument_expected_shortfall_raw(date = "2023-06-01",
#'                                                symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_instrument_expected_shortfall_raw <- function(date,
                                              symbols,
                                              horizon       = NULL,
                                              signif_level  = NULL,
                                              volatility_id = NULL){
  end_point <- "instrument-expected-shortfall-raw"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Delta Vector
#'
#' This function calls the 'instrument-delta-vector' endpoint of the EnvisionRisk API
#' to compute the delta vector for a set of financial instruments. The delta vector
#' provides
#' the partial derivatives of the financial instruments' value with respect to an
#' underlying
#' asset price, providing an indication of the sensitivity of the instruments' value to changes
#' in the asset price.
#'
#' @param date A string, the date at which the delta vector is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which the
#' delta vector is to be computed.
#' @param base_cur A string, the base currency in which the delta vector is to be computed.
#' @param horizon An integer, the time horizon over which the delta vector is computed.
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output delta vector data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' delta_vec <- envrsk_instrument_delta_vector(date = "2023-06-01",
#' symbols = c("AAPL", "GOOGL"))
#' }
envrsk_instrument_delta_vector <- function(date,
                                           symbols,
                                           base_cur      = NULL,
                                           horizon       = NULL,
                                           volatility_id = NULL){
  end_point <- "instrument-delta-vector"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Instrument Delta Vector (Raw)
#'
#' This function calls the 'instrument-delta-vector-raw' endpoint of the EnvisionRisk
#' API
#' to compute the delta vector for a set of financial instruments. The delta vector
#' provides
#' the partial derivatives of the financial instruments' value with respect to an
#' underlying
#' asset price, providing an indication of the sensitivity of the instruments' value
#' to changes
#' in the asset price.
#'
#' @param date A string, the date at which the delta vector is to be computed (YYYY-MM-DD).
#' @param symbols A list, the symbols of the financial instruments for which the delta
#' vector is to be computed.
#' @param horizon An integer, the time horizon over which the delta vector is computed.
#' @param volatility_id An integer, the ID of the volatility model to be used.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output delta vector data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' delta_vec <- envrsk_instrument_delta_vector_raw(date = "2023-06-01",
#' symbols = c("AAPL", "GOOGL"))
#' }
envrsk_instrument_delta_vector_raw <- function(date,
                                               symbols,
                                               horizon       = NULL,
                                               volatility_id = NULL){
  end_point <- "instrument-delta-vector-raw"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "horizon"       = horizon,
                  "volatility_id" = volatility_id)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}
#******************************************************************************
#### Time Series ####
#******************************************************************************
#' Market Price
#'
#' This function calls the 'market-price' endpoint of the EnvisionRisk API
#' to obtain the market prices for a set of financial instruments. The prices are
#' reported in the specified base currency.
#'
#' @param symbols A list, the symbols of the financial instruments for which the
#' market prices are to be obtained.
#' @param base_cur A string, the base currency in which the market prices are to
#' be reported.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output market price data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' market_prices <- envrsk_market_price(symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_market_price <- function(symbols,
                                base_cur = NULL){
  end_point <- "market-price"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("base_cur" = base_cur)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Market Price
#'
#' This function calls the 'market-price-raw' endpoint of the EnvisionRisk API
#' to obtain the market prices for a set of financial instruments. The prices are
#' reported in the specified base currency.
#'
#' @param symbols A list, the symbols of the financial instruments for which the
#' market prices are to be obtained.
#'
#' @return A list containing the input parameters, the time of operation,
#'         the output market price data, and details of mapped and unmapped symbols.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' market_prices <- envrsk_market_price_raw(symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_market_price_raw <- function(symbols){
  end_point <- "market-price-raw"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Market Volatility
#'
#' This function calls the 'market-volatility' endpoint of the EnvisionRisk API to
#' obtain the market volatilities for a set of financial instruments.
#'
#' @param symbols A list, the symbols of the financial instruments for which the
#' market volatilities are to be obtained.
#'
#' @return A list containing the input parameters, the time of operation, the output
#' market volatility data,
#'         and details of mapped and unmapped symbols. If the API call is unsuccessful,
#'         the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' market_volatilities <- envrsk_market_volatility(symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_market_volatility <- function(symbols){
  end_point <- "market-volatility"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#' Market Stress Volatility
#'
#' This function calls the 'market-stress-volatility' endpoint of the EnvisionRisk
#' API to
#' obtain the market stress volatilities for a set of financial instruments.
#'
#' @param symbols A list, the symbols of the financial instruments for which the
#' market stress volatilities are to be obtained.
#'
#' @return A list containing the input parameters, the time of operation, the output
#' market stress volatility data,
#'         and details of mapped and unmapped symbols. If the API call is unsuccessful,
#'         the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' stress_volatilities <- envrsk_market_stress_volatility(symbols = c("AAPL.US", "DANSKE.CO"))
#' }
envrsk_market_stress_volatility <- function(symbols){
  end_point <- "market-stress-volatility"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = symbols)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"            = out_raw[["Input"]],
      "tech_opr"         = Sys.time(),
      "Output"           = data.table::rbindlist(out_raw[["Output"]], fill=TRUE),
      "symbols_mapped"   = data.table::rbindlist(out_raw[["Symbols_Mapped"]], fill = TRUE),
      "symbols_unmapped" = data.table::rbindlist(out_raw[["Symbols_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }
  return(out)
}

#******************************************************************************
#### Manifest ####
#******************************************************************************
#' Get Manifest
#'
#' This function calls the 'get-manifest' endpoint of the EnvisionRisk API to
#' obtain the manifest, which includes available parameters for portfolio construction
#' and risk computation.
#'
#' @return If the API call is successful, a list containing the manifest data is
#' returned,
#'         with portfolio constituents combined into a single vector.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' manifest <- envrsk_get_manifest()
#' }
envrsk_get_manifest <- function(){
  end_point <- "get-manifest"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list()

  res_out <- envrsk_get(url          = api_url,
                        access_token = get_access_token(),
                        params       = .params)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]
    out_raw[["PORT_CONSTITUENTS"]] <- do.call(c, out_raw[["PORT_CONSTITUENTS"]])
  } else {
    return(res_out)
  }
  return(out_raw)
}

#' Update a Manifest
#'
#' This function makes an HTTP POST request to the 'put-manifest' API endpoint
#' to update the specified manifest.
#'
#' @param manifest A list, data frame or other structure that holds the manifest
#'    to be updated. This must match the format expected by the API.
#'
#' @return NULL If the API request is successful (i.e., HTTP status code 200),
#'    this function prints a success message and returns NULL. If the API request
#'    is not successful, this function returns the full response from the API.
#'
#' @examples
#' \dontrun{
#'   # Update a manifest with data
#'   manifest <- list(id = "abc123", status = "complete")
#'   envrsk_update_manifest(manifest)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' my_manifest              <- envrsk_get_manifest()
#' my_manifest$BASE_CUR     <- "DKK"
#' my_manifest$SIGNIF_LEVEL <- 0.975
#'
#' envrsk_update_manifest(my_manifest)
#' }
envrsk_update_manifest <- function(manifest){
  end_point <- "put-manifest"
  api_url   <- get_api_url(end_point)

  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = list(manifest))

  if(res_out[["status_code"]] == 200){
    message("OK - Manifest updated")
  } else {
    return(res_out)
  }
}

#' envrsk_manifest_restore_to_default
#'
#' This function restores the default configuration of the manifest
#' by sending a POST request to a specific API endpoint.
#' If the status code of the response is 200, it prints a success message.
#' Otherwise, it returns the whole response.
#'
#' @usage envrsk_manifest_restore_to_default()
#'
#' @details
#' This function constructs the API url using the `get_api_url` helper function
#' with "restore-manifest" as the endpoint.
#' It then prepares the query parameters and removes any parameters
#' with zero length by indexing with `lengths(.params) != 0`.
#' It sends a POST request to the `api_url` using the `envrsk_post` helper function.
#' The function requires no explicit arguments from the user, but utilizes
#' an access token generated by the `get_access_token` helper function.
#' It assumes a default body content (`list(manifest)`).
#'
#' @return
#' If the status code of the response is 200, the function will not
#' return a value but print a message: "OK - Manifest updated".
#' If the status code is anything other than 200, the function will
#' return the complete response object.
#'
#' @examples
#' \dontrun{
#'   envrsk_manifest_restore_to_default()
#' }
#'
#' @export
envrsk_manifest_restore_to_default <- function(){
  end_point <- "restore-manifest"
  api_url   <- get_api_url(end_point)

  # Query parameters
  .params <- list()
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_get(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params)

  if(res_out[["status_code"]] == 200){
    message("OK - Manifest restored")
  } else {
    return(res_out)
  }
}

#******************************************************************************
#### Workflow ####
#******************************************************************************
#' envrsk_workflow_backtest function
#'
#' This function conducts a backtest for VaR and ES predictions.
#'
#' @param backtestdata A dataset used for backtesting. This data is sent as body content in the API call.
#' @param base_cur Optional. A character string specifying the base currency used for VaR, ES and the returns. Default is NULL.
#' @param signif_level Optional. A numeric value specifying the significance level used for the calculation of VaR & ES. Default is NULL.
#'
#' @return If the API call is successful (i.e., status code 200), the function returns a list containing 'Title',
#' 'Input' (which includes 'backtestdata', 'base_cur', 'signif_level'), 'TechOpr', and 'Output'. If the API call fails,
#' it returns the original output from the API call which includes the status code and error message.
#'
#' @examples
#' \dontrun{
#' # Download a test dataset with daily VaR and ES predictions and daily returns.
#' # The VaR and ES prediction are based on a confidence level of 97,5% with 1-day risk horizon.
#' # VaR, ES and the returns are all dinominated in DKK.
#' dt_backtest_data <- readRDS(url("https://www.dropbox.com/s/owhjtmd2xlzft8s/backtestdata.rds?raw=true","rb"))
#'
#' # Process the backtest
#' result_backtest <- envrsk_workflow_backtest(backtestdata = dt_backtest_data,
#'                                             base_cur     = "DKK",
#'                                             signif_level = 0.975)
#' }
#'
#' @export
envrsk_workflow_backtest <- function(backtestdata,
                                     base_cur      = NULL,
                                     signif_level  = NULL){

  end_point <- "workflow-backtest"
  api_url   <- get_api_url(end_point)

  .params <- list("base_cur"      = base_cur,
                  "signif_level"  = signif_level)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = backtestdata)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list("Title"  = out_raw[["Title"]],
                "Input"  = list("backtestdata" = data.table::rbindlist(out_raw[["Input"]][["BacktestData"]]),
                                "base_cur"     = out_raw[["Input"]][["BaseCur"]],
                                "signif_level" = out_raw[["Input"]][["SignifLevel"]]),
                "TechOpr"  = out_raw[["TechOpr"]],
                "Output"   = data.table::rbindlist(out_raw[["Output"]]))
  } else {
    return(res_out)
  }

  return(out)
}

#' Run Workflow Risk Snapshot
#'
#' This function runs a workflow risk snapshot which calculates risk measures for a portfolio of positions.
#' The function makes an API request to the specified endpoint and processes the returned data. It achieves
#' this by making a post request to the specified endpoint with a variety of parameters that allow the user
#' to customize the level of detail and perspective of the risk report. The function returns a list of various
#' outputs, including the original input data, a technical operations report, portfolio positions, a
#' portfolio delta vector, a portfolio risk report, and information on the mapping of positions.
#'
#' @param date          The date for which to run the risk snapshot.
#' @param positions     The positions data.
#' @param base_cur      The base currency to use for the risk calculations. Default is NULL.
#' @param horizon       The horizon for the risk calculations. Default is NULL.
#' @param signif_level  The significance level for the risk calculations. Default is NULL.
#' @param volatility_id The volatility id to use for the risk calculations. Default is NULL.
#' @param risk_measure  The risk_measure signify what risk measure to use in the report. Options are 'VaR' or 'ES'. Default is NULL ('ES' is the used).
#' @param report_depth  The depth of the report to be generated. Default is NULL.
#' @param simplify      Logical. If TRUE, the output is simplified. Default is FALSE.
#'
#' @return A list containing the inputs, technical operations, positions, portfolio delta vector,
#'         portfolio risk, mapped positions, and unmapped positions.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#'                                                        "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                    "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' response_risk_snapshot_1 <- envrsk_workflow_risk_snapshot(date      = Sys.Date(),
#'                                                         positions = dt_positions)
#'
#' response_risk_snapshot_2 <- envrsk_workflow_risk_snapshot(date      = Sys.Date(),
#'                                                    positions    = dt_positions,
#'                                                    base_cur     = "USD",
#'                                                    horizon      = 10,
#'                                                    signif_level = 0.99,
#'                                                    volatility_id = "downturn",
#'                                                    simplify = TRUE)
#' }
envrsk_workflow_risk_snapshot <- function(date,
                                          positions,
                                          base_cur      = NULL,
                                          horizon       = NULL,
                                          signif_level  = NULL,
                                          volatility_id = NULL,
                                          risk_measure  = NULL,
                                          report_depth  = NULL,
                                          simplify      = FALSE){
  end_point <- "workflow-risk-snapshot"
  api_url <- get_api_url(end_point)

  # Query parameters
  .params <- list("date"          = date,
                  "base_cur"      = base_cur,
                  "horizon"       = horizon,
                  "signif_level"  = signif_level,
                  "volatility_id" = volatility_id,
                  "risk_measure"  = risk_measure,
                  "report_depth"  = report_depth)
  .params <- .params[lengths(.params) != 0]

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = .params,
                         body         = positions)

  if(res_out[["status_code"]] == 200){
    out_raw <- res_out[["content"]]

    out <- list(
      "Input"                  = out_raw[["Input"]],
      "tech_opr"               = out_raw[["tech_opr"]],
      "positions"              = data.table::rbindlist(out_raw[["positions"]]),
      "portfolio_delta_vector" = data.table::rbindlist(out_raw[["portfolio_delta_vector"]]),
      "portfolio_risk"         = data.table::rbindlist(out_raw[["portfolio_risk"]]),
      "positions_mapped"       = data.table::rbindlist(out_raw[["Positions_Mapped"]], fill = TRUE),
      "positions_unmapped"     = data.table::rbindlist(out_raw[["Positions_UnMapped"]], fill = TRUE)
    )
  } else {
    return(res_out)
  }

  return(out)
}

#******************************************************************************
#### Miscellaneous ####
#******************************************************************************
#' Decorate Portfolio with Product Type
#'
#' This function calls the 'decorate-table-id' endpoint of the EnvisionRisk API
#' to enrich portfolio position data with additional information based on position ID.
#'
#' @param positions A list of positions to be enriched with additional information.
#' @param simplify Logical, indicating whether to simplify the output. If TRUE, o
#' nly the output content is returned.
#'        If FALSE, the entire API response is returned. Default is TRUE.
#'
#' @return If the API call is successful, a list containing the enriched positions
#' is returned.
#'         If the API call is unsuccessful, the original API response is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' dt_positions_without_product_type <- as.data.frame(list("symbol"   = c("AAPL.US", "DANSKE.CO",
#'                                                                        "CashUSD", "AGG.US"),
#'                                    "quantity" = c(129, 768, 69000, 89)))
#' dt_positions <- envrsk_decorate_with_product_type(positions = dt_positions_without_product_type)
#' }
envrsk_decorate_portfolio_with_product_type <- function(positions, simplify = TRUE){
  end_point <- "decorate-position-id"
  api_url   <- get_api_url(end_point)

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = list(),
                         body         = positions)

  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
  return(out)
}

#' Decorates the portfolio with IDs using envrsk API
#'
#' This function uses the envrsk API to decorate a given portfolio with the
#' respective ID. The decoration process typically includes the addition of
#' meta-data, additional risk factors or calculations based on the portfolio
#' positions.
#'
#' @param positions Data Frame or List. The portfolio positions that should
#' be decorated. This input should align with the envrsk API requirements for
#' the `decorate-table-id` endpoint.
#' @param simplify Logical. If TRUE, the function only returns the 'Output' field
#' of the API response as a data.table. If FALSE, the function returns the
#' entire response object. Default is TRUE.
#' @return If the API call is successful (HTTP status code 200) and `simplify = TRUE`,
#' a data.table from the 'Output' field of the API response is returned. If
#' `simplify = FALSE`, the entire response object is returned. If the API call
#' is not successful, the function returns the API response with an error message.
#' @export
#' @examples
#' \dontrun{
#' dt_positions <- as.data.frame(list("symbol"        = c("AAPL.US", "DANSKE.CO",
#'                                                        "CashUSD", "AGG.US"),
#'                                    "position_type" = c("single_stock",
#'                                                        "single_stock", "cash", "etf"),
#'                                    "quantity"      = c(129, 768, 69000, 89)))
#'
#' dt_positions <- envrsk_decorate_portfolio_with_uid(dt_positions)
#' }
envrsk_decorate_portfolio_with_uid <- function(positions, simplify = TRUE){
  end_point <- "decorate-table-id"
  api_url   <- get_api_url(end_point)

  res_out <- envrsk_post(url          = api_url,
                         access_token = get_access_token(),
                         params       = list(),
                         body         = positions)
  if(res_out[["status_code"]] == 200){
    out <- res_out[["content"]]

    if(!is.null(out[["Output"]])){
      out[["Output"]] <- data.table::rbindlist(out[["Output"]], fill = TRUE)
    }

    if(simplify){
      return(out[["Output"]])
    } else {
      return(out)
    }
  } else {
    return(res_out)
  }
  return(out)
}
