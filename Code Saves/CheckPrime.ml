# Lecture 3

# Recursion and Tail Calls

- CPU doesn’t know what a function is, just executes a series of instructions
- Call Stack is memory that keeps track of function calls
    - Pushes current instruction address on stack
    - Pushes arguments
    - Jump to code of function
    - To return: pop all arguments and return address

```ocaml
let is_prime n =
  if n < 2 then failwith "no can do" else
    let rec go (i : int) : bool = 
  (* idea: i starts at 2 and we're counting up, so we're initially calling
     `is_prime 2 n` for some n.
     Along the way, we check if `n mod i = 0` i.e. whether i evenly
     divides n. If it does, then n isn't prime; else make a recursive call
     increasing `i`. *)
      if i = n then true else 
    (* if we counted all the way up to `n`, then we ensured that 2...(n-1) don't
    divide n, so n is prime. *)
    (* up to you to code the rest! *)
      if n mod i = 0 then false else
        go (n+1)
    in
    go 2
```

- Tracing shows every step of computation until value is reached
    - End up with long stack frame
        - 5 + (4 + (3 + (2 + (1 + sum(0)))))
    - Instead, tail calls
        - Recycles stack frames
- **Tail call optimization** (TCO) recycled stack frame of current function
- TCO can be done on any recursive algorithm, and enablese running in **constant stack space**
    - Often introduce extra parameter, called **************accumulator**************
- Tail-recursive

```ocaml
let rec sum n partial_sum =
  if n = 0 then partial_sum else sum (n-1) (partial_sum + n)
```

- Type synonyms
    - type name = string
    - type height = int
- Tuples
    - Collection of values of various types
    - type person = name * height
    - let ada : person = (”ada”, 100)
