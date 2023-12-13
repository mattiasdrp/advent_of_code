def rotate(lines)
  lines.map(&:chars).transpose.map(&:join)
end

def min(val1, val2)
  val1 < val2 ? val1 : val2
end

def check_symetry(lines)
  prev_line = ""
  lines.each_with_index.find do |line, index|
    if prev_line == line
      height = lines.length
      min_range = min(index - 1, height - index - 1)
      (1..min_range).all? do |i|
        lines[index + i] == lines[index - i - 1]
      end
    else
      prev_line = line
      false
    end
  end
end

def parse_block(block)
  lines = block.split
  _, res = check_symetry(lines)
  return 100 * res unless res.nil?

  rotated = rotate(lines)
  check_symetry(rotated) => [_, res]

  res
end

def part1(file)
  File.readlines(file, "\n\n", chomp: true).reduce(0) { |sum, block| sum + parse_block(block) }
end

class String
  def hamming_distance(other)
    (0..size - 1).reduce(0) do |sum, i|
      sum + (self[i] != other[i] ? 1 : 0)
    end
  end
end

def check_symetry_hamming(lines)
  prev_line = ""
  lines.each_with_index.find do |line, index|
    init_dist = line.hamming_distance(prev_line)
    prev_line = line
    if init_dist <= 1
      height = lines.length
      min_range = min(index - 1, height - index - 1)
      total = (1..min_range).reduce(init_dist) do |sum, i|
        sum + lines[index + i].hamming_distance(lines[index - i - 1])
      end
      total == 1
    else
      false
    end
  end
end

def parse_block2(block)
  lines = block.split
  _, res = check_symetry_hamming(lines)
  return 100 * res unless res.nil?

  rotated = rotate(lines)
  check_symetry_hamming(rotated) => [_, res]
  res
end

def part2(file)
  File.readlines(file, "\n\n", chomp: true).reduce(0) { |sum, block| sum + parse_block2(block) }
end

puts(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
