### STAT 5814 HW1/PROBLEM 4(c)
### AUTHOR: SAYANTAN MAJUMDAR
### EMAIL: smxnv@mst.edu
### SNO: 12566087

sigma = 1
num_samples = 500
eps = rnorm(n=num_samples, m=0, s=sigma)
Y = c(1: length(eps))
time_steps = c(1: length(eps))
theta_set = c(-1, 0.5, 1)
for (theta in theta_set) {
  for (t in 2: length(time_steps)) {
    Y[t] = eps[t] - theta * eps[t-1] ^ 2
  }
  plot(time_steps, Y, type='l', xlab='t', ylab='Y(t)', main=paste('Simulated Y(t) for theta=', theta, 'and n=', num_samples))
}
