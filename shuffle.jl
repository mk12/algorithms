# Copyright 2015 Mitchell Kember. Subject to the MIT License.

using Gadfly
using Base.Test

# Plots the distribution of permutations for shuffling n cards with the custom
# shuffling function, using the given number of bins in the histogram. Returns
# the Gadfly plot object.
function distribution(n, samples, bins, shuffle)
  perms = Array(Int, samples)
  for i = 1:samples
    perms[i] = perm_index(shuffle([0:n-1]))
  end
  plot(
    x=perms, Geom.histogram(bincount=bins),
    yintercept=[samples/bins], Geom.hline(color="yellowgreen"),
    Guide.title(""), Guide.xlabel(""), Guide.ylabel(""),
    Scale.y_continuous(minvalue=0)
  )
end

# Saves the distribution as an SVG file with the given name.
function save_svg(dist, name)
  draw(SVG(name, 6inch, 4inch), dist)
end

# Returns a random number between a and b inclusive.
function next_rand(a, b)
  rand(a:b)
end

# Shuffles xs in-place using a bad algorithm.
function simple_shuffle!(xs)
  n = length(xs)
  for _ = 1:n
    a = next_rand(1, n)
    b = next_rand(1, n)
    xs[a], xs[b] = xs[b], xs[a]
  end
  xs
end

# Shuffles xs in-place using a good algorithm.
function knuth_shuffle!(xs)
  n = length(xs)
  for i = 1:n
    j = next_rand(i, n)
    xs[i], xs[j] = xs[j], xs[i]
  end
  xs
end

# Identifies the permutation of [0:n-1] that xs corresponds to, where n is the
# length of xs. Returns zero for ascending order and n!-1 for desceding order.
# Does not check that xs is a permutation.
function perm_index(xs)
  if isempty(xs)
    return 0
  end
  n = length(xs)
  available = ones(xs)
  index = 0
  f = factorial(n-1)
  for i = 1:n-1
    val = xs[i]
    pos = 0
    for j = 1:val
      pos += available[j]
    end
    index += pos * f
    available[val+1] = 0
    f /= n - i
  end
  index
end

# Verifies permutation indices for permutations of [0] through [0:3].
function test_perm_index()
  order = Any[
    [0],
    [0 1; 1 0],
    [0 1 2; 0 2 1; 1 0 2; 1 2 0; 2 0 1; 2 1 0],
    [0 1 2 3; 0 1 3 2; 0 2 1 3; 0 2 3 1; 0 3 1 2; 0 3 2 1;
     1 0 2 3; 1 0 3 2; 1 2 0 3; 1 2 3 0; 1 3 0 2; 1 3 2 0;
     2 0 1 2; 2 0 2 1; 2 1 0 3; 2 1 3 0; 2 3 0 1; 2 3 1 0;
     3 0 1 2; 3 0 2 1; 3 1 0 2; 3 1 2 0; 3 2 0 1; 3 2 1 0]
  ]
  for ps in order
    for i = 1:size(ps, 1)
      @test perm_index(ps[i,:]) == i - 1
    end
  end
end

# Plots the distributions for the simple shuffle and for the Knuth shuffle, and
# saves each to an SVG file.
function main()
  const n = 20
  const samples = 1_000_000
  const bins = 200
  save_svg(distribution(n, samples, bins, simple_shuffle!), "simple.svg")
  save_svg(distribution(n, samples, bins, knuth_shuffle!), "knuth.svg")
end
