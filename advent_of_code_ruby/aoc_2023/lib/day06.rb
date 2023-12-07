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
  delta = Math.sqrt(time**2 - 4 * dist)
  x1 = ((time - delta) / 2 + 1).floor
  x2 = ((time + delta) / 2 - 1).ceil
  x2 - x1 + 1
end

def compute(file, part)
  total = 1
  parse(file, part).each do |pair|
    total *= combinations(pair)
  end
  total
end

puts compute(ARGV[0], ARGV[1].to_i)
