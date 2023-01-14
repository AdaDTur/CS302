let rec sum n partial_sum =
  if n = 0 then partial_sum else sum (n-1) (partial_sum + n)
