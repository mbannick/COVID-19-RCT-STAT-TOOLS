# source('~/covid/prob/r/code.r')
source('~/COVID-19-RCT-STAT-TOOLS/Simulation/time-to-event/prob/r/code.r')
task_id = Sys.getenv("SLURM_ARRAY_TASK_ID")
## sink(file = paste('log', task_id, '.txt', sep=''))
nn <- paste('/fh/scratch/delete30/gao_f/mbannick/tte-output/prob/out/out', task_id, '.rda', sep = '')
## sink()
if(!file.exists(nn)){
  try(assign(paste('r', task_id, sep = ''), try(funslave(as.numeric(task_id)))))
  save.image(paste('/fh/scratch/delete30/gao_f/mbannick/tte-output/prob/out/out', task_id, '.rda', sep = ''))
} else {
  print("file already exists.")
}
quit('no')
