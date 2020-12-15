# Functions list
# getopt()
# get_Rscript_filename()
# get_Me()
# cal_hsq(bfile, out, pheno, ,prevalence, distribution)


# Default
currentPATH = getwd()
plink = '/public/home/xuhm/bin/plink/plink'
plink2 = '/public/home/xuhm/bin/plink2/plink2'
gear = 'java -jar -Xmx30720M /public/home/xuhm/bin/gear.jar'

# Function: getopt()
# Package: getopt
# Copyright (c) 2008-2010 Allen Day
# Copyright (c) 2011-2018 Trevor L. Davis <trevor.l.davis@gmail.com>
library(stats)
getopt = function (spec=NULL,opt=NULL,command=get_Rscript_filename(),usage=FALSE,debug=FALSE) {

  # littler compatibility - map argv vector to opt
  if (is.null(opt)) {
    if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
      opt = get("argv", envir = .GlobalEnv) #nocov
    } else {
      opt = commandArgs(TRUE)
    }
  }
  
  ncol=4
  maxcol=6
  col.long.name    = 1
  col.short.name   = 2
  col.has.argument = 3
  col.mode         = 4
  col.description  = 5
  
  flag.no.argument = 0
  flag.required.argument = 1
  flag.optional.argument = 2
  
  result = list()
  result$ARGS = vector(mode="character")
  
  #no spec.  fail.
  if ( is.null(spec) ) {
    stop('argument "spec" must be non-null.')
    
    #spec is not a matrix.  attempt to coerce, if possible.  issue a warning.
  } else if ( !is.matrix(spec) ) {
    if ( length(spec)/4 == as.integer(length(spec)/4) ) {
      warning('argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion')
      spec = matrix( spec, ncol=ncol, byrow=TRUE )
    } else {
      stop('argument "spec" must be a matrix, or a character vector with length divisible by 4, rtfm.')
    }
    
    #spec is a matrix, but it has too few columns.
  } else if ( dim(spec)[2] < ncol ) {
    stop(paste('"spec" should have at least ', ncol, ' columns.',sep=''))
    
    #spec is a matrix, but it has too many columns.
  } else if ( dim(spec)[2] > maxcol ) {
    stop(paste('"spec" should have no more than ', maxcol, ' columns.',sep=''))
    
    #spec is a matrix, and it has some optional columns.
  } else if ( dim(spec)[2] != ncol ) {
    ncol = dim(spec)[2]
  }
  
  #sanity check.  make sure long names are unique, and short names are unique.
  if ( length(unique(spec[,col.long.name])) != length(spec[,col.long.name]) ) {
    stop(paste('redundant long names for flags (column ',col.long.name,' of spec matrix).',sep=''))
  }
  if ( length(stats::na.omit(unique(spec[,col.short.name]))) != length(stats::na.omit(spec[,col.short.name])) ) {
    stop(paste('redundant short names for flags (column ',col.short.name,' of spec matrix).',sep=''))
  }
  # convert numeric type to double type
  spec[,4] <- gsub("numeric", "double", spec[,4])
  
  # if usage=TRUE, don't process opt, but generate a usage string from the data in spec
  if ( usage ) {
    ret = ''
    ret = paste(ret,"Usage: ",command,sep='')
    for ( j in 1:(dim(spec))[1] ) {
      ret = paste(ret,' [-[-',spec[j,col.long.name],'|',spec[j,col.short.name],']',sep='')
      if (spec[j,col.has.argument] == flag.no.argument) {
        ret = paste(ret,']',sep='')
      } else if (spec[j,col.has.argument] == flag.required.argument) {
        ret = paste(ret,' <',spec[j,col.mode],'>]',sep='')
      } else if (spec[j,col.has.argument] == flag.optional.argument) {
        ret = paste(ret,' [<',spec[j,col.mode],'>]]',sep='')
      }
    }
    # include usage strings
    if ( ncol >= 5 ) {
      max.long = max(apply(cbind(spec[,col.long.name]),1,function(x)length(strsplit(x,'')[[1]])))
      ret = paste(ret,"\n",sep='')
      for (j in 1:(dim(spec))[1] ) {
        ret = paste(ret,sprintf(paste("    -%s|--%-",max.long,"s    %s\n",sep=''),
                                spec[j,col.short.name],spec[j,col.long.name],spec[j,col.description]
        ),sep='')
      }
    }
    else {
      ret = paste(ret,"\n",sep='')
    }
    return(ret)
  }
  
  #XXX check spec validity here.  e.g. column three should be convertible to integer
  
  i = 1
  
  while ( i <= length(opt) ) {
    if ( debug ) print(paste("processing",opt[i]))
    
    current.flag = 0 #XXX use NA
    optstring = opt[i]
    
    
    #long flag
    if ( substr(optstring, 1, 2) == '--' ) {
      if ( debug ) print(paste("  long option:",opt[i]))
      
      optstring = substring(optstring,3)
      
      this.flag = NA
      this.argument = NA
      kv = strsplit(optstring, '=')[[1]]
      # if ( !is.na(kv[2]) ) {
      if ( grepl('=', optstring) ) {
        this.flag = kv[1]
        this.argument = paste(kv[-1], collapse="=")
      } else {
        this.flag = optstring
      }
      
      rowmatch = grep( this.flag, spec[,col.long.name],fixed=TRUE )
      
      #long flag is invalid, matches no options
      if ( length(rowmatch) == 0 ) {
        stop(paste('long flag "', this.flag, '" is invalid', sep=''))
        
        #long flag is ambiguous, matches too many options
      } else if ( length(rowmatch) > 1 ) {
        # check if there is an exact match and use that
        rowmatch = which(this.flag == spec[,col.long.name])
        if(length(rowmatch) == 0) {
          stop(paste('long flag "', this.flag, '" is ambiguous', sep=''))
        }
      }
      
      #if we have an argument
      if ( !is.na(this.argument) ) {
        #if we can't accept the argument, bail out
        if ( spec[rowmatch, col.has.argument] == flag.no.argument ) {
          stop(paste('long flag "', this.flag, '" accepts no arguments', sep=''))
          
          #otherwise assign the argument to the flag
        } else {
          mode = spec[rowmatch, col.mode]
          warning_msg <- tryCatch(storage.mode(this.argument) <- mode,
                                  warning = function(w) {warning(paste(mode, "expected, got", dQuote(this.argument)))})
          if( is.na(this.argument) && !grepl("expected, got",  warning_msg) ) {
            warning(paste('long flag', this.flag, 'given a bad argument'))
          }
          result[spec[rowmatch, col.long.name]] = this.argument
          i = i + 1
          next
        }
        
        #otherwise, we don't have an argument
      } else {
        #if we require an argument, bail out
        ###if ( spec[rowmatch, col.has.argument] == flag.required.argument ) {
        ###  stop(paste('long flag "', this.flag, '" requires an argument', sep=''))
        
        #long flag has no attached argument. set flag as present.  set current.flag so we can peek ahead later and consume the argument if it's there
        ###} else {
        result[spec[rowmatch, col.long.name]] = TRUE
        current.flag = rowmatch
        ###}
      }
      
      #short flag(s)
    } else if ( substr(optstring, 1, 1) == '-' ) {
      if ( debug ) print(paste("  short option:",opt[i]))
      
      these.flags = strsplit(optstring,'')[[1]]
      
      done = FALSE
      for ( j in 2:length(these.flags) ) {
        this.flag = these.flags[j]
        rowmatch = grep( this.flag, spec[,col.short.name],fixed=TRUE )
        
        #short flag is invalid, matches no options
        if ( length(rowmatch) == 0 ) {
          stop(paste('short flag "', this.flag, '" is invalid', sep=''))
          
          #short flag has an argument, but is not the last in a compound flag string
        } else if ( j < length(these.flags) & spec[rowmatch,col.has.argument] == flag.required.argument ) {
          stop(paste('short flag "', this.flag, '" requires an argument, but has none', sep=''))
          
          #short flag has no argument, flag it as present
        } else if ( spec[rowmatch,col.has.argument] == flag.no.argument ) {
          result[spec[rowmatch, col.long.name]] = TRUE
          done = TRUE
          
          #can't definitively process this flag yet, need to see if next option is an argument or not
        } else {
          result[spec[rowmatch, col.long.name]] = TRUE
          current.flag = rowmatch
          done = FALSE
        }
      }
      if ( done ) {
        i = i + 1
        next
      }
    }
    
    #invalid opt
    if ( current.flag == 0 ) {
      stop(paste('"', optstring, '" is not a valid option, or does not support an argument', sep=''))
      #TBD support for positional args
      #if ( debug ) print(paste('"', optstring, '" not a valid option.  It is appended to getopt(...)$ARGS', sep=''))
      #result$ARGS = append(result$ARGS, optstring)
      
      # some dangling flag, handle it
    } else if ( current.flag > 0 ) {
      if ( debug ) print('    dangling flag')
      if ( length(opt) > i ) {
        peek.optstring = opt[i + 1]
        if ( debug ) print(paste('      peeking ahead at: "',peek.optstring,'"',sep=''))
        
        #got an argument.  attach it, increment the index, and move on to the next option.  we don't allow arguments beginning with '-' UNLESS
        #specfile indicates the value is an "integer" or "double", in which case we allow a leading dash (and verify trailing digits/decimals).
        if ( substr(peek.optstring, 1, 1) != '-' |
             #match negative double
             ( substr(peek.optstring, 1, 1) == '-'
               & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek.optstring) > 0
               & spec[current.flag, col.mode]== 'double'
             ) |
             #match negative integer
             ( substr(peek.optstring, 1, 1) == '-'
               & regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", peek.optstring) > 0
               & spec[current.flag, col.mode]== 'integer'
             )
        ) {
          if ( debug ) print(paste('        consuming argument *',peek.optstring,'*',sep=''))
          
          # storage.mode(peek.optstring) = spec[current.flag, col.mode]
          mode = spec[current.flag, col.mode]
          tryCatch(storage.mode(peek.optstring) <- mode,
                   warning = function(w) {warning(paste(mode, "expected, got", dQuote(peek.optstring)))})
          result[spec[current.flag, col.long.name]] = peek.optstring
          i = i + 1
          
          #a lone dash
        } else if ( substr(peek.optstring, 1, 1) == '-' & length(strsplit(peek.optstring,'')[[1]]) == 1 ) {
          if ( debug ) print('        consuming "lone dash" argument')
          # storage.mode(peek.optstring) = spec[current.flag, col.mode]
          mode = spec[current.flag, col.mode]
          tryCatch(storage.mode(peek.optstring) <- mode,
                   warning = function(w) {warning(paste(mode, "expected, got", dQuote(peek.optstring)))}) #nocov 
          result[spec[current.flag, col.long.name]] = peek.optstring
          i = i + 1
          
          #no argument
        } else {
          if ( debug ) print('        no argument!')
          
          #if we require an argument, bail out
          if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
            stop(paste('flag "', this.flag, '" requires an argument', sep=''))
            
            #otherwise set flag as present.
          } else if (
            spec[current.flag, col.has.argument] == flag.optional.argument |
            spec[current.flag, col.has.argument] == flag.no.argument 
          ) {
            x = TRUE
            storage.mode(x) = spec[current.flag, col.mode]
            result[spec[current.flag, col.long.name]] = x
          } else {
            stop(paste("This should never happen.", #nocov
                       "Is your spec argument correct?  Maybe you forgot to set", #nocov
                       "ncol=4, byrow=TRUE in your matrix call?")) #nocov
          }
        }
        #trailing flag without required argument
      } else if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
        stop(paste('flag "', this.flag, '" requires an argument', sep=''))
        
        #trailing flag without optional argument
      } else if ( spec[current.flag, col.has.argument] == flag.optional.argument ) {
        x = TRUE
        storage.mode(x) = spec[current.flag, col.mode]
        result[spec[current.flag, col.long.name]] = x
        
        #trailing flag without argument
      } else if ( spec[current.flag, col.has.argument] == flag.no.argument ) {
        x = TRUE
        storage.mode(x) = spec[current.flag, col.mode]
        result[spec[current.flag, col.long.name]] = x
      } else {
        stop("this should never happen (2).  please inform the author.") #nocov
      }
    } #no dangling flag, nothing to do.
    
    i = i+1
  }
  return(result)
}

get_Rscript_filename <- function() {
  prog <- sub("--file=", "", grep("--file=", commandArgs(), value=TRUE)[1])
  if( .Platform$OS.type == "windows") { 
    prog <- gsub("\\\\", "\\\\\\\\", prog)
  }
  prog
}

get_Me = function(){
  a_melog = system('tail -n 5 Me.log | head -n 1',intern = T) # plink output 
  #d_melog = system('tail -n 7 dMe.log | head -n 1',intern = T) # gcta output
  #w_melog = system('tail -n 7 wMe.log | head -n 1',intern = T) # gcta output
  
  a_me = as.numeric(sub(" ","",strsplit(a_melog,':')[[1]][2]))
  #d_me = 1/as.numeric(sub(" ","",strsplit(d_melog,'=')[[1]][2]))
  #w_me = 1/as.numeric(sub(" ","",strsplit(w_melog,'=')[[1]][2]))
  d_me = 0
  w_me = 0

  cat(paste0("\nAdditive Me is ",a_me,'.\nDominance Me is ',d_me,'.\nWeighted Me is ',w_me,'.\n'))
  
  Me = c(a_me, d_me, w_me)
  return(Me)
}

check_status = function(process_name = NULL){
  status = system(paste0("qstat | grep ", process_name, " | awk '{print $5}'"), intern = T)
  if (length(status) == 0) return(TRUE)
  if (status == "C") return(TRUE)
  else return(FALSE)
}

PBS_keep = function(bfile = NULL, samplingsize = NULL, fam_data = NULL){
  if (is.null(samplingsize)){
    totalsize = nrow(fam_data)
    samplingsize = floor(totalsize/10)
  }
  small_sample = fam_data[,1:2][sample(1:totalsize, samplingsize), ]
  sample_ID_file = paste0(samplingsize, 'sampleID.txt')
  write.table(small_sample, sample_ID_file, quote = F, col.names = F, row.names = F)
  
  plink_keep_cmd = paste0(plink, ' --bfile ', bfile, ' --keep ', sample_ID_file, ' --make-bed --out MeSample')
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('KEEP_',currentTime[[1]][1], currentTime[[1]][2])
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=1\n#PBS -l mem=20G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  pbs = paste0(pbs_header, plink_keep_cmd, '\n')
  write.table(pbs, 'sampleKeep.sh', quote = F, col.names = F, row.names = F)
  
  system('qsub sampleKeep.sh')
  
  return(pbs_name)
}

PBS_a_me = function(){
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('MeA_',currentTime[[1]][1], currentTime[[1]][2])
  
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  plink_grm_cmd = paste0(plink, ' --bfile MeSample', ' --make-grm-gz --threads 20 --out MeSample')
  gear_cmd = paste0(gear, ' gstat --grm MeSample --out Me')
  pbs = paste0(pbs_header, '\n', plink_grm_cmd, '\n', gear_cmd)
  write.table(pbs, 'MeCal.sh', quote = F, col.names = F, row.names = F)
  
  system('qsub MeCal.sh')
  return(pbs_name)
}

PBS_d_me = function(){
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('MeD_',currentTime[[1]][1], currentTime[[1]][2])
  
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  gcta_d = paste0('/public/home/xuhm/bin/gcta64 --bfile MeSample --thread-num 20 --make-grm-d-gz --out dMe')
  pbs = paste0(pbs_header, '\n', gcta_d)
  write.table(pbs, 'dMeCal.sh', quote = F, col.names = F, row.names = F)
  
  system('qsub dMeCal.sh')
  return(pbs_name)
}

PBS_w_me = function(){
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('MeW_',currentTime[[1]][1], currentTime[[1]][2])
  
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  gcta_w = paste0('/public/home/xuhm/bin/gcta64 --bfile MeSample --thread-num 20 --make-grm-gz --make-grm-alg 1 --out wMe')
  pbs = paste0(pbs_header, '\n', gcta_w)
  write.table(pbs, 'wMeCal.sh', quote = F, col.names = F, row.names = F)
  
  system('qsub wMeCal.sh')
  return(pbs_name)
}

PBS_ppc = function(bfile = NULL){
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('PPC_',currentTime[[1]][1], currentTime[[1]][2])
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  plink_pca_cmd = paste0(plink2, ' --bfile MeSample --pca allele-wts 2 --freq counts --out MeSample')
  plink_projectPC_cmd = paste0(plink2, ' --bfile ', bfile, ' --read-freq MeSample.acount --score MeSample.eigenvec.allele 2 5 header-read no-mean-imputation variance-standardize --score-col-nums 6-7 --out PPC')
  
  pbs = paste0(pbs_header, plink_pca_cmd, '\n', plink_projectPC_cmd, '\n')
  write.table(pbs, 'PPC.sh', quote = F, col.names = F, row.names = F)
  
  system('qsub PPC.sh')
  return(pbs_name)
}

generate_covariate = function(fam_data = NULL){
  ppc = read.table('PPC.sscore', header = F)[,c(2,5,6)]
  colnames(fam_data)[c(1,2,5)] = c('FID', 'IID', 'Gender')
  colnames(ppc) = c('IID', 'ppc1', 'ppc2')
  cov_dt = merge(ppc, fam_data[,c(1,2,5)], 'IID')
  cov_dt = cov_dt[,c(4,1:3,5)]
  
  covar_file = 'covariate.cov'
  write.table(cov_dt, 'covariate.cov', quote = F, col.names = F, row.names = F)
  return(covar_file)
}

PBS_glmA = function(bfile = NULL, pheno = NULL, mpheno = NULL, covar = NULL, out = 'Heritability'){
  if (is.null(mpheno)){
    plink2_add_cmd = paste0(plink2, ' --bfile ', bfile, ' --glm hide-covar --pheno ', pheno, ' --covar ', covar, ' --out ', out, 'A')
  } else {
    plink2_add_cmd = paste0(plink2, ' --bfile ', bfile, ' --glm hide-covar --pheno ', pheno, ' --mpheno ', mpheno,' --covar ', covar, ' --out ', out, 'A')
  }
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('GLM_A_',currentTime[[1]][1], currentTime[[1]][2])
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  pbs = paste0(pbs_header, plink2_add_cmd, '\n')
  write.table(pbs, 'GLM_A.sh', quote = F, col.names = F, row.names = F)
  system('qsub GLM_A.sh')
  
  return(pbs_name)
}

PBS_glmAD = function(bfile = NULL, pheno = NULL, mpheno = NULL, covar = NULL, out = 'Heritability'){
  if (is.null(mpheno)){
    plink2_dom_cmd = paste0(plink2, ' --bfile ', bfile, ' --glm hide-covar genotypic --pheno ', pheno, ' --covar ', covar, ' --out ', out, 'AD')
  } else {
    plink2_dom_cmd = paste0(plink2, ' --bfile ', bfile, ' --glm hide-covar genotypic --pheno ', pheno, ' --mpheno ', mpheno,' --covar ', covar, ' --out ', out, 'AD')
  }
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('GLM_AD_',currentTime[[1]][1], currentTime[[1]][2])
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=20\n#PBS -l mem=40G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  pbs = paste0(pbs_header, plink2_dom_cmd, '\n')
  write.table(pbs, 'GLM_AD.sh', quote = F, col.names = F, row.names = F)
  system('qsub GLM_AD.sh')
  
  return(pbs_name)
}

PBS_frq = function(bfile = NULL){
  currentTime = strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')
  pbs_name = paste0('FRQ_',currentTime[[1]][1], currentTime[[1]][2])
  pbs_header = paste0("#PBS -N ", pbs_name, "\n#PBS -l nodes=1:ppn=1\n#PBS -l mem=30G\n#PBS -l walltime=6666:00:00\n#PBS -j oe\ncd $PBS_O_WORKDIR\n")
  
  plink_frq = paste0(plink, ' --bfile ', bfile, ' --freq --out ', bfile)
  pbs = paste0(pbs_header, plink_frq, '\n')
  write.table(pbs, 'freq.sh', quote = F, col.names = F, row.names = F)
  system('qsub freq.sh')
  
  return(pbs_name)
}

cal_hsq = function(bfile = NULL, out = 'Heritability', pheno = NULL, prevalenceFile = NULL, distribution = NULL){
  freq = read.table(paste0(bfile, '.frq'), header = T) # for weighted hsq
  Me = get_Me()
  a_me = Me[1]
  #d_me = Me[2]
  #w_me = Me[3]
  
  hsq = vector()
  #hsq_d = vector()
  #w_hsq = vector()
  
  se_hsq = vector()
  #se_hsq_d = vector()
  #w_se_hsq = vector()
  
  sample_num = vector()
  case_proportion = vector()
  
  if (distribution == 'linear'){
    for (i in 1:phe_num){
      addfile = paste0(out,'A', ".PHENO", i, ".glm.linear")
      #domfile = paste0(out,'AD', ".PHENO", i, ".glm.linear")
      
      #process_trait_D = read.table(domfile, header = F)
      
      ADD = read.table(addfile, header = F)
      #DOM = process_trait_D[process_trait_D[,7] == 'DOMDEV',]
      
      ADD = na.omit(merge(ADD, freq[,c(2,5)], by.x = 'V3', by.y = 'SNP'))
      #DOM = na.omit(merge(DOM, freq[,c(2,5)], by.x = 'V3', by.y = 'SNP'))
      
      ss=mean(ADD[,11]^2, na.rm=T)
      #ssd=mean(DOM[,11]^2, na.rm=T)
      sz=mean(ADD[,8], na.rm=T)
      FQ = ADD$MAF
      # hsq
      h2=(ss-1)/(sz/a_me)
      #h2d=(ssd-1)/(sz/d_me)
      #w_h2 = (sum(2*FQ*(1-FQ)*(ADD[,11]^2))/sum(2*FQ*(1-FQ))-1)*w_me/sz
      # hsq variance calculator based on RPubs/H2ss/Delta Methods
      se_h2 = sqrt(2*a_me/(sz^2)+2*(a_me^3)*(h2^2)/(sz^5))
      #se_h2d = sqrt(2*d_me/(sz^2)+2*(d_me^3)*(h2d^2)/(sz^5))
      #se_w_h2 = sqrt(2*w_me/(sz^2)+2*(w_me^3)*(w_h2^2)/(sz^5))

      sample_num = c(sample_num, floor(sz))
      hsq = c(hsq, h2)
      #hsq_d = c(hsq_d, h2d)
      #w_hsq = c(w_hsq, w_h2)
      se_hsq = c(se_hsq, se_h2)
      #se_hsq_d = c(se_hsq_d, se_h2d)
      #w_se_hsq = c(w_se_hsq, se_w_h2)
    }
    #outDT = cbind(pheno_names, sample_num, a_me, d_me, w_me, hsq, hsq_d, w_hsq, se_hsq, se_hsq_d, w_se_hsq)
    #colnames(outDT) = c('FieldID','N','A_Me','D_Me', 'W_Me', 'A_hsq','D_hsq', 'W_hsq', 'A_hse','D_hse', 'W_hse')
	outDT = cbind(pheno_names, sample_num, a_me, hsq, se_hsq)
    colnames(outDT) = c('FieldID','N','A_Me', 'A_hsq', 'A_hse')
  } else if (distribution == 'binary'){
    phenoFile = read.table(pheno, header = F)
    # calculate case proportion
    for (i in 3:ncol(phenoFile)){
      frq = table(phenoFile[,i])
      case_proportion = c(case_proportion, frq[[2]]/(frq[[1]]+frq[[2]]))
    }
    
    for (i in 1:phe_num){
      addfile = paste0(out,'A', ".PHENO", i, ".glm.logistic.hybrid")
      domfile = paste0(out,'AD', ".PHENO", i, ".glm.logistic.hybrid")
      
      process_trait_D = read.table(domfile, header = F)
      
      ADD = read.table(addfile, header = F)
      DOM = process_trait_D[process_trait_D[,8] == 'DOMDEV',]
      
      ADD = na.omit(merge(ADD, freq[,c(2,5)], by.x = 'V3', by.y = 'SNP'))
      DOM = na.omit(merge(DOM, freq[,c(2,5)], by.x = 'V3', by.y = 'SNP'))
      
      ss=mean(ADD[,12]^2, na.rm=T)
      ssd=mean(DOM[,12]^2, na.rm=T)
      sz=mean(ADD[,9], na.rm=T)
      FQ = ADD$MAF
      # hsq
      h02=(ss-1)/(sz/a_me)
      h02d=(ssd-1)/(sz/d_me)
      w_h02 = (sum(2*FQ*(1-FQ)*(ADD[,12]^2))/sum(2*FQ*(1-FQ))-1)*w_me/sz
    
      # Liability scale
      K = prevalenceFile[,2][i]
      P = case_proportion[i]
      z = dnorm(qnorm(K))
      
      h2_liability = h02*((K*(1-K))^2)/(z^2)/(P*(1-P))
      h2d_liability = h02d*((K*(1-K))^2)/(z^2)/(P*(1-P))
      w_h2_liability = w_h02*((K*(1-K))^2)/(z^2)/(P*(1-P))
    
      se_h2 = sqrt(2*a_me/(sz^2)+2*(a_me^3)*(h2_liability^2)/(sz^5))
      se_h2d = sqrt(2*d_me/(sz^2)+2*(d_me^3)*(h2d_liability^2)/(sz^5))
      se_w_h2 = sqrt(2*w_me/(sz^2)+2*(w_me^3)*(w_h2_liability^2)/(sz^5))
      
      sample_num = c(sample_num, floor(sz))
      hsq = c(hsq, h2_liability)
      hsq_d = c(hsq_d, h2d_liability)
      w_hsq = c(w_hsq, w_h2_liability)
      se_hsq = c(se_hsq, se_h2)
      se_hsq_d = c(se_hsq_d, se_h2d)
      w_se_hsq = c(w_se_hsq, se_w_h2)
    }
    outDT = cbind(pheno_names, sample_num, prevalenceFile[,2], case_proportion, a_me, d_me, w_me, hsq, hsq_d, w_hsq, se_hsq, se_hsq_d, w_se_hsq)
    colnames(outDT) = c('FieldID','N','prevalence','case_proportion','A_Me','D_Me', 'W_Me', 'A_hsq','D_hsq', 'W_hsq', 'A_hse','D_hse', 'W_hse')
  } else if (distribution == 'categorical'){
    # Upcoming!
  }
  
  write.table(outDT, paste0(phe_num, distribution, '.hsq'), quote = F,col.names = T,row.names = F)
  cat(paste0('All results written to ', phe_num, distribution, '.hsq.\n'))
}
