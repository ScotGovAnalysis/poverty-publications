
# replace categories with other categories 
decode <- function(x, search, replace, default = NULL) {
  # build a nested ifelse function by recursion
  
  decode.fun <- function(search, replace, default = NULL)
    
    if (length(search) == 0L) {function(x) if (is.null(default)) x else rep(default, length(x))} 
  else {function(x) ifelse(x == search[1L], 
                           replace[1L],
                           decode.fun(tail(search,  -1L),
                                      tail(replace, -1L),
                                      default)(x))}
  
  return(decode.fun(search, replace, default)(x))
}

# get flags for poverty outcomes and some hhld level charateristics 
gethhworkstatus <- function(df){
  
  # get household level work status
  workinghh <- df %>%
    mutate(working = ifelse(ecobu %in% c("1", "2", "3", "4", "5"), 1, 0)) %>%
    group_by(sernum) %>%
    summarise(workinghh = max(working))
  
  df %>%
    left_join(workinghh, by = "sernum")
}

gethhdisabledstatus <- function(df){
  
  # get household level disability status
  disabledhh <- df %>%
    group_by(sernum) %>%
    summarise(disch_hh = max(discorkid),
              disad_hh = max(discorabflg)) %>%
    mutate(disch_hh = ifelse(disch_hh > 0, 1, 0),
           disad_hh = ifelse(disad_hh >0, 1, 0),
           dispp_hh = ifelse(disch_hh + disad_hh > 0, 1, 0 ))
  
  df %>%
    left_join(disabledhh, by = "sernum")
}

# get flags for poverty outcomes
getpovertyflags <- function(df){
  
  df %>%
    mutate(abspovahc = ifelse(s_oe_ahc*infl_ahc < abspovahc_threshold, 1, 0),
           abspovbhc = ifelse(s_oe_bhc*infl_bhc < abspovbhc_threshold, 1, 0),
           workpovahc = ifelse(low60ahc == 1 & workinghh == 1, 1, 0),
           workpovbhc = ifelse(low60bhc == 1 & workinghh == 1, 1, 0),
           cmdahc = ifelse(low70ahc == 1 & mdch == 1, 1, 0),
           cmdahc_new = ifelse(low70ahc == 1 & mdchnew == 1, 1, 0),
           cmdbhc = ifelse(low70bhc == 1 & mdch == 1, 1, 0),
           cmdbhc_new = ifelse(low70bhc == 1 & mdchnew == 1, 1, 0)) 
}

# get urban/rural flags from FRS househol dataset
geturbanrural <- function(df){
  
  # get the correct FRS househol dataset (current year is stored in "comment" attribute)
  urindshh <- househol[[comment(df)]]
  
  # remove some attributes to avoid warnings
  attr(urindshh$sernum, "format.sas") <- NULL
  attr(urindshh$sernum, "label") <- NULL
  attr(df$sernum, "format.sas") <- NULL
  attr(df$sernum, "label") <- NULL
  
  # join with hbai dataset
  df %>%
    left_join(urindshh, by = "sernum") %>%
    mutate(urinds = factor(urinds, levels = urbrurcodes, labels = urbrurclasses))
}

# get poverty numbers and rates for children, dults, pensioners, and people 
getpov <- function(df, ...){
  
  df %>%
    filter(gvtregn == "Scotland") %>%
    mutate(chn = sum(gs_newch),
           wan = sum(gs_newwa),
           pnn = sum(gs_newpn),
           ppn = sum(gs_newpp)) %>%
    group_by(...) %>%
    summarise(chnum = sum(gs_newch),
              wanum = sum(gs_newwa),
              pnnum = sum(gs_newpn),
              ppnum = sum(gs_newpp),
              chn = max(chn),
              wan = max(wan),
              pnn = max(pnn),
              ppn = max(ppn)) %>%
    mutate(chrate = chnum/chn,
           warate = wanum/wan,
           pnrate = pnnum/pnn,
           pprate = ppnum/ppn) %>%
    select(chnum, chrate, wanum, warate, 
           pnnum, pnrate, ppnum, pprate, ...) %>%
    filter(.[[9]] == 1)
  
}

# Add year variabale to table
addyearvar <- function(df){
  
  df %>%
    rownames_to_column(var = "years") %>%
    select(years, everything())
}

# Format population numbers and rates
fmtpop <- function(x){
  
  require(scales)
  
  comma(x, 10000)
}

fmtpct <- function(x){
  
  require(scales)
  
  percent(x, 1)
}

formatpov <- function(df){
  
  df %>%
    mutate(years = factor(years, levels = years, labels = years_formatted)) %>%
    mutate_at(vars(ends_with("num")), fmtpop) %>%
    mutate_at(vars(ends_with("rate")), fmtpct) %>%
    select(1:8)
}
