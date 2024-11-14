
pak::pak("future")
pak::pak("furrr")
pak::pak("tictoc")

tictoc::tic()
.scoreAlignment(ac_alig@alignment)
tictoc::toc()
# Benchmark 03.09 = 12.989s





#### Playiong with future
future::plan("multisession")
tictoc::tic()
f <- future::future(.scoreAlignment(ac_alig@alignment))
future::value(f)
tictoc::toc()



#### Look into list envs for faster computations
