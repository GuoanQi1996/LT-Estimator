# NOTE
# Make sure PBS mission name is shorter than 16 characters
# Header check before read data into R is sometimes very important, as R treat header line with "#" at the beganing as no header
# ppc results scaled ?
source('ToolFunc.r')
# Parameters
command = matrix(c("bfile","b",1,"character", "plink binary file",
                   "pheno","p",1,"character", "phenotypic file",
                   "prevalence","pr",1,"character","prevalence of binary traits, NA for continuous traits",
                   "distribution","d",1,"character","specify distribution of the phenotype, linear, binary or categorical (upcoming)",
                   "covar","c",2,"character", "covariate file, optional, default top 2 project PC and gender will be generated and used as covariates", 
                   "effectivemarker","e",0,"logical","specify if both additive and dominance Me is present in Me.log, standrad output of GEAR",
                   "samplingsize","s",2,"integer", "sampling a small population for Me calculation, optional 1/10 of total sample size",
                   "mpheno","m",2,"integer", "index of the phenotypic file, optional, all phenotype analysis for null", 
                   "lastStepOnly","l",0,"logical", "no parameter accept, when GLM and Me are both present, specify this command",
                   "out","o",2,"character","specify outfile name of PLINK GLM, optional, defaulted Heritability",
                   "help","h",0,"logical", "parameters input instruction"),
                 byrow=T,ncol=5)
args = getopt(spec = command)

if (!is.null(args$help) || is.null(args$bfile) || is.null(args$pheno) || is.null(args$prevalence) || is.null(args$distribution)) {
  cat(paste(getopt(command, usage = T), "\n"))
  q()
}

# grab arguements
bfile = args$bfile
pheno = args$pheno
prevalence = args$prevalence
distribution = args$distribution
out = paste0(distribution, 'Heritability')
me = NULL
covar = NULL
mpheno = NULL
samplingsize = NULL
lastStepOnly = NULL


if (!is.null(args$mpheno)) {
  mpheno = args$mpheno
}
if (!is.null(args$out)) {
  out = args$out
}
if (!is.null(args$covar)) {
  covar = args$covar
}
if (!is.null(args$effectivemarker)) {
  me = TRUE
}
if (!is.null(args$lastStepOnly)) {
  lastStepOnly = TRUE
}

# read prevalence and merge it with present phenotype
prevalenceFile = read.table(prevalence, header = F)
pheno_names = read.table(paste0(sub('....$', '', pheno),'.names'), header = F)

pheno_names[,1] = as.character(pheno_names[,1])
for (i in 1:nrow(pheno_names)){
  pheno_names[,1][i] = strsplit(pheno_names[,1][i],split = '[.]')[[1]][2]
}
prevalenceFile = merge(pheno_names, prevalenceFile, 'V1', sort = F)
phe_num = nrow(prevalenceFile)
# read individual info
fam_data = read.table(paste0(bfile, '.fam'), header = F)

if (!is.null(lastStepOnly)) { # means all Me and chi-square statistics are present
  if(!file.exists(paste0(bfile,".frq"))) {
    job_frq = PBS_frq(bfile)
    while (1){
      if(check_status(job_frq)) break
      else Sys.sleep(120)
    }
  }
  cal_hsq(bfile, out, pheno, prevalenceFile, distribution)
} else {
  job_frq = PBS_frq(bfile)
  if (!is.null(covar)){
    job_glmA = PBS_glmA(bfile, pheno, mpheno, covar, out)
    job_glmAD = PBS_glmAD(bfile, pheno, mpheno, covar, out)
  } else {
    job_keep = PBS_keep(bfile, samplingsize, fam_data)
    while (1){
      if(check_status(job_keep)) break
      else Sys.sleep(120)
    }
    if (is.null(me)) {
      job_a_me = PBS_a_me()
      job_d_me = PBS_d_me()
      job_w_me = PBS_w_me()
    }
    
    job_ppc = PBS_ppc(bfile)
    while (1){
      if(check_status(job_ppc)) break
      else Sys.sleep(120)
    }
    
    covar = generate_covariate(fam_data)
    
    job_glmA = PBS_glmA(bfile, pheno, mpheno, covar, out)
    job_glmAD = PBS_glmAD(bfile, pheno, mpheno, covar, out)
  }
  # check_all_status
  while(1) {
    if (check_status(job_frq) & check_status(job_a_me) & check_status(job_d_me) & check_status(job_w_me) & check_status(job_glmA) & check_status(job_glmAD)) break
    else Sys.sleep(120)
  }
  cal_hsq(bfile, out, pheno, prevalenceFile, distribution)
}