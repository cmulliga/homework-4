##############################################################################
## Read in CMS payments, rebates, and risk scores for each plan */
##############################################################################

## Assign yearly datasets and clean variables


## 2010
ma.path.2010a=paste0("data/input/cms-payment-data/2010/2010PartCPlanLevel2.xlsx")
risk.rebate.2010a=read_xlsx(ma.path.2010a,range="A4:H3157",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc",
                                        "msa_deposit_partc"))

ma.path.2010b=paste0("data/input/cms-payment-data/2010/2010PartDPlans2.xlsx")
risk.rebate.2010b=read_xlsx(ma.path.2010b,range="A4:H4417",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


## 2011
ma.path.2011a=paste0("data/input/cms-payment-data/2011/2011PartCPlanLevel.xlsx")
risk.rebate.2011a=read_xlsx(ma.path.2011a,range="A4:H2663",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc",
                                        "msa_deposit_partc"))

ma.path.2011b=paste0("data/input/cms-payment-data/2011/2011PartDPlans.xlsx")
risk.rebate.2011b=read_xlsx(ma.path.2011b,range="A4:H3561",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


## 2012
ma.path.2012a=paste0("data/input/cms-payment-data/2012/2012PartCPlanLevel.xlsx")
risk.rebate.2012a=read_xlsx(ma.path.2012a,range="A4:H2757",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc",
                                        "msa_deposit_partc"))

ma.path.2012b=paste0("data/input/cms-payment-data/2012/2012PartDPlans.xlsx")
risk.rebate.2012b=read_xlsx(ma.path.2012b,range="A4:H3605",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


## 2013
ma.path.2013a=paste0("data/input/cms-payment-data/2013/2013PartCPlan Level.xlsx")
risk.rebate.2013a=read_xlsx(ma.path.2013a,range="A4:G2968",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc"))

ma.path.2013b=paste0("data/input/cms-payment-data/2013/2013PartDPlans.xlsx")
risk.rebate.2013b=read_xlsx(ma.path.2013b,range="A4:H3836",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


## 2014
ma.path.2014a=paste0("data/input/cms-payment-data/2014/2014PartCPlan Level.xlsx")
risk.rebate.2014a=read_xlsx(ma.path.2014a,range="A4:G2828",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc"))

ma.path.2014b=paste0("data/input/cms-payment-data/2014/2014PartDPlans.xlsx")
risk.rebate.2014b=read_xlsx(ma.path.2014b,range="A4:H3902",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


## 2015
ma.path.2015a=paste0("data/input/cms-payment-data/2015/2015PartCPlanLevel.xlsx")
risk.rebate.2015a=read_xlsx(ma.path.2015a,range="A4:G2745",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "riskscore_partc","payment_partc","rebate_partc"))

ma.path.2015b=paste0("data/input/cms-payment-data/2015/2015PartDPlans.xlsx")
risk.rebate.2015b=read_xlsx(ma.path.2015b,range="A4:H3755",
                            col_names=c("contractid","planid","contract_name","plan_type",
                                        "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                        "costsharing_partd"))


for (y in 2010:2015) {
  risk.rebate.a=get(paste("risk.rebate.",y,"a",sep="")) %>%
    mutate_at(vars(c("riskscore_partc","payment_partc","rebate_partc")),
              ~as.numeric(str_replace_all(.,'/$',''))) %>%
    mutate(planid=as.numeric(planid), year=y) %>%
    select(contractid, planid, contract_name, plan_type, riskscore_partc,
           payment_partc, rebate_partc, year)
    

  risk.rebate.b=get(paste("risk.rebate.",y,"b",sep="")) %>%
    mutate(payment_partd=directsubsidy_partd + reinsurance_partd + costsharing_partd) %>%
    mutate(planid=as.numeric(planid)) %>%
    select(contractid, planid, payment_partd, directsubsidy_partd, reinsurance_partd, costsharing_partd,
           riskscore_partd)
  
  risk.rebate = risk.rebate.a %>%
    left_join(risk.rebate.b, by=c("contractid","planid"))

  assign(paste("risk.rebate.",y,sep=""),risk.rebate)
  
}

risk.rebate.final=rbind(risk.rebate.2010,risk.rebate.2011,
                        risk.rebate.2012,risk.rebate.2013,risk.rebate.2014,
                        risk.rebate.2015)
write_rds(risk.rebate.final,"data/output/risk_rebate.rds")
