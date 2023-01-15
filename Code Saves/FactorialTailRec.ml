let factorial num =
  let rec go j acc =
    if j > 0 then go (j - 1) (acc * j) else acc
  in
  go num 1
