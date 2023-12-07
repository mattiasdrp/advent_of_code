# frozen_string_literal: true

def parse(file, part)
  file = File.open(file, mode: "r")
  times = file.readline.scan(/\d+/)
  distances = file.readline.scan(/\d+/)
  if part == 1
    times.map(&:to_i).zip(distances.map(&:to_i))
  else
    [[times.join("").to_i, distances.join("").to_i]]
  end
end

def combinations((time, dist))
  # We solve f(t) = t * (time - t) >= dist
  delta = time**2 - 4 * dist
  x1 = (time - Math.sqrt(delta)) / 2
  x2 = (time + Math.sqrt(delta)) / 2
  x1 += 1 if x1 == x1.to_i || x2 == x2.to_i
  x2.to_i - x1.to_i
end

def compute(file, part)
  total = 1
  parse(file, part).each do |pair|
    total *= combinations(pair)
  end
  total
end

puts compute(ARGV[0], ARGV[1].to_i)
