# frozen_string_literal: true

def parse(line, (crow, ccol), points, part)
  direction, distance =
    if part == 1
      matching = /(\w) (\d+) \(#([\d\w]{6})\)/.match line
      [matching[1], matching[2].to_i]
    else
      matching = /\w \d+ \(#([\d\w]{5})(\d)\)/.match line
      direction = case matching[2]
                  in "0"
                    "R"
                  in "1"
                    "D"
                  in "2"
                    "L"
                  in "3"
                    "U"
                  else
                  end
      distance = matching[1].to_i(16)
      [direction, distance]
    end
  case direction
  in "R"
    points[[crow, ccol]] = [crow, ccol + distance]
    [crow, ccol + distance]
  in "L"
    points[[crow, ccol]] = [crow, ccol - distance]

    [crow, ccol - distance]
  in "U"
    points[[crow, ccol]] = [crow - distance, ccol]

    [crow - distance, ccol]
  in "D"
    points[[crow, ccol]] = [crow + distance, ccol]
    [crow + distance, ccol]
  end
end

def shoelace_formula(points)
  (points.sum(0) { |(row1, col1), (row2, col2)| (row1 + row2) * (col1 - col2) }) / 2
end

def part(file, part)
  points = {}
  File.readlines(file, chomp: true).reduce([0, 0]) { |pos, line| parse(line, pos, points, part) }
  boundary = points.sum(0) { |(row1, col1), (row2, col2)| (row2 - row1).abs + (col2 - col1).abs }
  area = shoelace_formula(points)

  # Pick's theorem : area = inside + boundary/2 - 1
  # since we know the area and the boundary
  inside = area - boundary / 2 + 1

  boundary + inside
end

puts(ARGV[1] == "1" ? part(ARGV[0], 1) : part(ARGV[0], 2))
