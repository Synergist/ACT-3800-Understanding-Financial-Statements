getFinancialDataFromZacks <- function(ticker) {
  library(rvest)
  address_prefix <- 'https://www.zacks.com/stock/quote/'
  cash.flow <- read_html(paste0(address_prefix, ticker, '/cash-flow-statements'))
  cash.flow.from.ops <- html_table(cash.flow %>% html_node(xpath='//*[@id="cash_flow_operation"]/table'))
  cash.flow.funds.usage <- html_table(cash.flow %>% html_node(xpath='//*[@id="cash_flow_use"]/table'))
  cash.flow.from.ops <- cash.flow.from.ops[-1,]
  colnames(cash.flow.from.ops)[1] <- 'Measurement'
  colnames(cash.flow.funds.usage)[1] <- 'Measurement'
  data.cash.flow <- rbind(cash.flow.from.ops, cash.flow.funds.usage)
  data.cash.flow$FinancialSheet <- 'CashFlowStatement'
  data.cash.flow.melt <- melt(data.cash.flow, id.vars = c('Measurement', 'FinancialSheet'))
  
  balance.sheet <- read_html(paste0(address_prefix, ticker, '/balance-sheet'))
  balance.assets <- html_table(balance.sheet %>% html_node(xpath='//*[@id="annual_income_statement"]/table[1]'))
  colnames(balance.assets)[1] <- 'Measurement'
  # balance.assets$SheetName <- 'Assets'
  balance.assets <- balance.assets[-1,]
  balance.liabs.equity <- html_table(balance.sheet %>% html_node(xpath='//*[@id="annual_income_statement"]/table[2]'))
  colnames(balance.liabs.equity)[1] <- 'Measurement'
  # balance.liabs.equity$SheetName = 'Liabilities & Shareholders Equity'
  balance.equity <- html_table(balance.sheet %>% html_node(xpath='//*[@id="annual_income_statement"]/table[3]'))
  colnames(balance.equity)[1] <- 'Measurement'
  # balance.equity$SheetName <- 'Shareholders Equity'
  data.balance.sheet <- rbind(balance.assets, balance.liabs.equity, balance.equity)
  data.balance.sheet$FinancialSheet <- 'BalanceSheet'
  data.balance.sheet.melt <- melt(data.balance.sheet, id.vars = c('Measurement', 'FinancialSheet'))
  
  income.statement <- read_html(paste0(address_prefix, ticker, '/income-statement'))
  income.tbl1 <- html_table(income.statement %>% html_node(xpath='//*[@id="annual_income_statement"]/table[1]'))
  colnames(income.tbl1)[1] <- 'Measurement'
  income.tbl2 <- html_table(income.statement %>% html_node(xpath='//*[@id="annual_income_statement"]/table[2]'))
  colnames(income.tbl2)[1] <- 'Measurement'
  income.tbl3 <- html_table(income.statement %>% html_node(xpath='//*[@id="annual_income_statement"]/table[3]'))
  colnames(income.tbl3)[1] <- 'Measurement'
  data.income.statement <- rbind(income.tbl1, income.tbl2, income.tbl3)
  data.income.statement$FinancialSheet <- 'IncomeStatement'
  data.income.statement.melt <- melt(data.income.statement, id.vars = c('Measurement', 'FinancialSheet'))
  
  data.combined <- rbind(data.cash.flow.melt, data.balance.sheet.melt, data.income.statement.melt)
  data.combined$StockTicker <- ticker
  return(data.combined)
}