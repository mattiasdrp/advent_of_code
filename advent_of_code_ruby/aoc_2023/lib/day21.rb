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

def update_pos(grid, row, col, new_pos, set)
  return if row.negative? || row >= grid.length || col.negative? || col >= grid[0].length || grid[row][col] == "#"

  set.add([row, col])
  new_pos << [row, col]
end

def part(file, steps)
  grid, start = parse(file)
  even_odd = [Set[], Set[]]
  gardener_pos = [start]
  visited = []
  memo = {}
  steps.times do |step|
    puts step if step % 1000 == 0
    curr, other = even_odd
    break unless memo[gardener_pos].nil?

    visited += gardener_pos
    new_pos = []
    gardener_pos.each do |(row, col)|
      update_pos(grid, row, col - 1, new_pos, curr)
      update_pos(grid, row, col + 1, new_pos, curr)
      update_pos(grid, row - 1, col, new_pos, curr)
      update_pos(grid, row + 1, col, new_pos, curr)
    end
    gardener_pos = new_pos.uniq - visited
    memo[gardener_pos] = true
    # pp gardener_pos
    # puts "----------------"
    even_odd = other, curr
  end
  odd, even = even_odd.map(&:to_a)
  # (0...grid.length).each do |row|
  #   (0...grid[0].length).each do |col|
  #     if odd.include?([row, col])
  #       print "O"
  #     else
  #       print grid[row][col]
  #     end
  #   end
  #   puts
  # end
  # (0...grid.length).each do |row|
  #   (0...grid[0].length).each do |col|
  #     if even.include?([row, col])
  #       print "O"
  #     else
  #       print grid[row][col]
  #     end
  #   end
  #   puts
  # end
  [even.length, odd.length]
end

puts(ARGV[1] == "1" ? part(ARGV[0], 64) : part(ARGV[0], 26_501_365))
