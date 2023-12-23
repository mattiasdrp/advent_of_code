def parse(file)
  grid = File.readlines(file, chomp: true).map(&:chars)
  start = catch :coords do
    (0..grid.length).each do |row|
      (0..grid[0].length).each do |col|
        throw :coords, [row, col] if grid[row][col] == "S"
      end
    end
  end
  [grid, start]
end

def update_pos(part, grid, row, col, new_pos, set)
  return if
    (part == 1 && (row.negative? || row >= grid.length || col.negative? || col >= grid[0].length)) ||
    grid[row % grid.length][col % grid[0].length] == "#"

  new_pos << [row, col]
  !set.add?([row, col]).nil?
end

def even_odd_cycle(part, file, steps)
  grid, start = parse(file)
  gardener_pos = [start]
  visited = []
  _, curr, steps = (0...steps).reduce([Set[], Set[]]) do |even_odd, step|
    curr, other = even_odd
    visited += gardener_pos
    new_pos = []
    updated = gardener_pos.reduce(false) do |updated, (row, col)|
      updated |
        update_pos(part, grid, row, col - 1, new_pos, curr) |
        update_pos(part, grid, row, col + 1, new_pos, curr) |
        update_pos(part, grid, row - 1, col, new_pos, curr) |
        update_pos(part, grid, row + 1, col, new_pos, curr)
    end
    gardener_pos = new_pos.uniq - visited
    break [other, curr, step] unless updated

    [other, curr, steps]
  end
  curr.to_a.length
end

def part(part, file, steps)
  res = even_odd_cycle(part, file, steps)
  puts "#{res}"
end

if ARGV[1] == "1"
  part(1, ARGV[0], 64)

else
  part(2, ARGV[0], 65)
  part(2, ARGV[0], 196)
  part(2, ARGV[0], 327)
end

# Quadratic formula solution
# From : https://www.wolframalpha.com/input?i=quadratic+fit+calculator&assumption=%7B%22F%22%2C+%22QuadraticFitCalculator%22%2C+%22data3x%22%7D+-%3E%22%7B0%2C+1%2C+2%7D%22&assumption=%7B%22F%22%2C+%22QuadraticFitCalculator%22%2C+%22data3y%22%7D+-%3E%22%7B3691%2C+32975%2C+91439%7D%22
puts(3691 + 14_694 * 202_300 + 14_590 * (202_300**2))
