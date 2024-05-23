library(plumber)

root <- pr("my_services.R")
pr_run(
    root,
    host = "127.0.0.1",
    port = get_option_or_env("plumber.port", 12345)
)
